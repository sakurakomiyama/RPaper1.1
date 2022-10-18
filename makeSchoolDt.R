rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)




#================================#
####      Acoustic data       ####
#================================#


library(dplyr)
library(data.table)
library(stringr)
library(geojsonio)
library(broom)
library(rjson)
library(sf)
library(suncalc)


makeSchoolDt <- function (rawSv.dt, bottom.json, spdf, gps.dt) {
  Sv.dt <- rawSv.dt
  
  #== calculate depth_bin of pixel ==#
  setDT(Sv.dt)[, depth_bin := (DepthStop- DepthStart)/SampleCount]
  setDT(Sv.dt)[, depthStart := DepthStart + (depth_bin * (as.numeric(gsub("Sv", "", SampleNo))-1))]
  setDT(Sv.dt)[, depthStop := depthStart + depth_bin]
  
  #== calculate sV from SV ==#
  setDT(Sv.dt)[, sV := 10^(Sv/10)]
  
  #== frequency response by each ping & each sample  ==#
  setDT(Sv.dt)[, sV38 := sV[Frequency==38], by = c("id", "PingNumber", "SampleNo")]
  
  #== frequency response by school id use mean sV of school  ==#
  setDT(Sv.dt)[, sV_mean := mean(sV), by = c("id", "Frequency")][, rf := sV_mean/sV_mean[Frequency==38], by = c("id")]
  setDT(Sv.dt)[, sV_max := max(sV), by = c("id", "Frequency")][, sV_min := min(sV), by = c("id", "Frequency")]
  setDT(Sv.dt)[, PingNo := max(PingNumber)-min(PingNumber) + 1, by ="id"][, sV_stdv := sd(sV), by = c("id", "Frequency")]
  setDT(Sv.dt)[, SE_f := sV_stdv/sqrt(PingNo*SampleCount), by = c("id", "Frequency")][, SE := SE_f/mean(sV38), by = c("id", "Frequency")] #standard error
  setDT(Sv.dt)[, sV_var := var(sV), by = c("id", "Frequency")]
  setDT(Sv.dt)[, pixelNo := length(sV), by = c("id", "Frequency")]
  
  #== time ==#
  setDT(Sv.dt)[,da:=paste(stringr::str_sub(Date, 1,4), stringr::str_sub(Date, 5,6), stringr::str_sub(Date, 7,8), sep="-")]
  setDT(Sv.dt)[,Time:=as.integer(Time)][,Time := (ifelse(nchar(Time)==7, paste0("0", Time), Time))]
  setDT(Sv.dt)[,Time := (ifelse(nchar(Time)==6, paste0("00", Time), Time))][,Time := (ifelse(nchar(Time)==5, paste0("000", Time), Time))][,Time := (ifelse(nchar(Time)==4, paste0("0000", Time), Time))][,Time := (ifelse(nchar(Time)==3, paste0("00000", Time), Time))]
  setDT(Sv.dt)[,t:=paste(stringr::str_sub(Time, -8,-7), stringr::str_sub(Time, -6,-5), stringr::str_sub(Time, -4,-3), sep=":")][,t:=paste(t, stringr::str_sub(Time, -2,-1), sep=".")]
  setDT(Sv.dt)[,YMD_time:=paste(da, t, sep=" ")][,YMD_time:=as.POSIXct(YMD_time, "%Y-%m-%d %H:%M:%S", tz= "UTC")][,da:=NULL][,t:=NULL]
  
  #== Distance between pings ==#
  bottom.dt <- data.table(PingNumber = bottom.json$pingNumber,  PingDistance = as.numeric(bottom.json$vesselDistance), BottomDepth = as.numeric(bottom.json$lowerLayerBoundary))
  setDT(bottom.dt)[, Diff:=c(0, diff(bottom.dt$PingDistance))][, distance:= Diff*1852][,Diff:=NULL][,PingDistance:=NULL]
  
  #== School size(area), School length ==#
  Sv.dt <- merge(x = Sv.dt, y = bottom.dt, by = "PingNumber", all.x = TRUE)
  setDT(Sv.dt)[, pixel_size := distance*depth_bin]
  setDT(Sv.dt)[, school_area := sum(pixel_size), by = c("id", "Frequency")][,school_length := distance/.N, by = c("id", "Frequency","PingNumber")][, school_length := sum(school_length), by = c("id", "Frequency")]
  
  #== sA calculation ==#
  setDT(Sv.dt)[, sV_nmi := sV*4*pi*(1852)^2][, sV_nmi := sV_nmi * depth_bin]
  setDT(Sv.dt)[, sV_nmi_sample := mean(sV_nmi), by=c("id", "Frequency", "SampleNo")]#[, sA2 := sum(sV_nmi_sample)/PingNo, by=c("id", "Frequency")]
  setDT(Sv.dt)[, sA := sum(sV_nmi)/PingNo, by = c("id", "Frequency")]
  
  #== mean depth ==#
  setDT(Sv.dt)[, meanDepth := mean(c(DepthStart, DepthStop)), by=c("id", "Frequency")]
  setDT(Sv.dt)[, weighted_meanDepth := weighted.mean((depthStart+(depth_bin/2)),sV_nmi_sample), by=c("id", "Frequency", "PingNumber")]
  
  #== depth from bottom ==#
  setDT(Sv.dt)[, nor_Depth := weighted_meanDepth/BottomDepth][, DepthfromBottom := BottomDepth-DepthStop]
  setDT(Sv.dt)[, nor_DepthStart := DepthStart/BottomDepth][, nor_DepthStop := ifelse(DepthStop/max(BottomDepth)>1 , 1 , DepthStop/max(BottomDepth)), by=c("id","Frequency")]
  
  #== Perimeter ==#
  Sv.dt[, yoko := lapply(.SD, function(x) c(1, diff(x))), .SDcols = "SampleNo", by = list(id,Frequency, PingNumber)]
  Sv.dt[, yoko2 := ifelse(yoko!=1,distance*2, 0)][, yoko3 := sum(yoko2)+distance*2, by=c("id", "Frequency", "PingNumber")]
  Sv.dt[, yoko4 := yoko3/.N, by=c("id", "Frequency","PingNumber")][,yoko5:=sum(yoko4),  by=c("id", "Frequency")][,yoko:=NULL][,yoko2:=NULL][,yoko3:=NULL][,yoko4:=NULL]
  Sv.dt[, tate := c(1, diff(PingNumber)), by=c("id", "Frequency", "SampleNo")][, tate2 := ifelse(tate!=1, depth_bin*2, 0)][, tate3:=sum(tate2)+depth_bin*2, by=c("id", "Frequency", "SampleNo")]
  Sv.dt[, tate4 := tate3/.N, by=c("id", "Frequency", "SampleNo")][,tate5:=sum(tate4), by=c("id", "Frequency")][,tate:=NULL][,tate2:=NULL][,tate3:=NULL][,tate4:=NULL]
  Sv.dt[, Perimeter:= yoko5+tate5][,yoko5:=NULL][,tate5:=NULL]
  
  #== Fractal dimension ==#
  Sv.dt[, Fractal:= 2*(log(Perimeter / 4))/log(school_area) ]
  
  #== make r(f) data table ==#
  School.dt <- with(Sv.dt, aggregate(Sv.dt[,c("Latitude", "Longitude", "YMD_time", "weighted_meanDepth","nor_Depth","nor_DepthStart", "nor_DepthStop", "DepthfromBottom")], 
                                     list(id, category, Frequency, Date, PingNo, pixelNo, SampleCount,DepthStart, DepthStop, sV_mean,sV_max, sV_min, sV_stdv, sV_var, rf,SE, sA, school_area, school_length, meanDepth, Perimeter, Fractal, vessel), mean))
  
  School.dt <- as.data.table(School.dt)
  setnames(School.dt, c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5",
                        "Group.6", "Group.7","Group.8", "Group.9","Group.10",
                        "Group.11", "Group.12","Group.13", "Group.14", 
                        "Group.15","Group.16",  "Group.17", "Group.18","Group.19",
                        "Group.20", "Group.21", "Group.22", "Group.23"), 
           c("id", "category", "Frequency", "Date", "PingNo", 
             "pixelNo", "SampleCount","DepthStart", "DepthStop","sV_mean",
             "sV_max", "sV_min", "sV_stdv", "sV_var", "rf", 
             "SE","sA","school_area", "school_length", "meanDepth", 
             "Perimeter","Fractal", "vessel"))
  School.dt <- School.dt[order(id),]
  attr(School.dt$YMD_time, "tzone") <- "UTC"
  
  #== sun altitude ==#
  latlon <- subset(School.dt, select=c(Latitude, Longitude, YMD_time))
  colnames(latlon) <- c("lat", "lon", "date")
  altitude.df <- suncalc::getSunlightPosition(data=latlon, keep = c("altitude"))
  School.dt <- cbind(School.dt, altitude.df)
  School.dt <- subset(School.dt, select=-c(lat,lon, date))
  School.dt$altitude_degree <- School.dt$altitude*180/pi
  
  ##== add area ==##
  spdf <- mutate (spdf, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                          id=="3" ~ "Inner_Shoal_East_2016", id=="4" ~ "Inner_Shoal_North",
                                          id=="5" ~ "Inner_Shoal_West_2018", id=="6" ~ "Inner_Shoal_test",
                                          id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                          id=="9" ~ "Outer_Shoal", id=="10" ~ "VestbankenSouthEast",
                                          id=="11" ~ "VestbankenSouthWest", id=="12" ~ "Vestbanken_North",
                                          TRUE ~ "Vikingbanken"))
  
  test <- spdf %>% split(spdf$id) %>% 
    lapply(function(x) rbind(x,x[1,])) %>%
    lapply(function(x) x[,1:2]) %>%
    lapply(function(x) list(as.matrix(x))) %>%
    lapply(function(x) st_polygon(x))
  points <- st_as_sf(School.dt,coords=c('Longitude','Latitude'),remove = F)
  polys <- test %>% st_sfc() %>% st_sf(geom=.) %>% mutate(id=factor(1:13)) 
  temp <- polys  %>% st_intersection(points) 
  temp <- mutate (temp, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "VestbankenSouthEast",
                                          id=="3" ~ "VestbankenSouthWest", id=="4" ~ "Vestbanken_North",
                                          id=="5" ~ "Vikingbanken", id=="6" ~ "Engelsk_Klondyke",
                                          id=="7" ~ "Inner_Shoal_East_2016", id=="8" ~ "Inner_Shoal_North",
                                          id=="9" ~ "Inner_Shoal_West_2018", id=="10" ~ "Inner_Shoal_test",
                                          id=="11" ~ "Nordgyden", id=="12" ~ "Ostbanken",
                                          TRUE ~ "Outer_Shoal"))
  
  temp <- data.table(id=temp$id.1, area=temp$area)
  temp <- temp[!duplicated(temp[,c('id')]),] # the first area will be remain for data which are on a border of 2 areas 
  temp2 <- data.table(id=School.dt$id)
  t <- merge(x = temp2, y = temp, by = "id", all.x = TRUE)
  t[is.na(t$area)]$area = "outside"
  School.dt$area <- t$area
  
  
  #== school height, Elongation, Rectangularity =##
  setDT(School.dt)[, school_height:=DepthStop-DepthStart][, Elongation:=school_length/school_height]
  setDT(School.dt)[, school_rect:=(school_length*school_height)/school_area]
  setDT(School.dt)[, school_circ:=(Perimeter^2)/(4*pi*school_area)]
  
  
  #== add bottom depth ==#
  setDT(School.dt)[ ,bottom_Depth := weighted_meanDepth / nor_Depth]
  
  #== add coverage name ==#
  label <- unique(data.table(vessel = gps.dt$vessel, coverage_name = gps.dt$coverage_name, 
                             StartTime = gps.dt$StartTime, StopTime = gps.dt$StopTime,
                             distance_sum = gps.dt$distance_sum, area = gps.dt$area))
  setDT(School.dt)[setDT(label), on =. (YMD_time >= StartTime, 
                                        YMD_time <= StopTime,
                                        area == area, vessel == vessel), 
                   coverage_name := coverage_name]
  
  
  #== add original id  ==#
  School.dt <- as.data.table(School.dt)
  School.dt[, original_id := id]
  setDT(School.dt)[, id:= 1:.N, by=Frequency]
  
  
  return(School.dt)
  
}


#===================================================================#


rawSv.dt <- SvSchool_SD1032.dt #SvSchool_EROS.dt, SvSchool_SD1031.dt, SvSchool_SD1032.dt

bottom.json <- rjson::fromJSON(file="C:/Data/S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json")
#C:/Data/S2019847_PEROS_3317/EXPORT/bottomEchogramPlot_T20190423_16561743-20190512_19512565.json
#C:/Data/S2019_SAILDRONE_1031/EXPORT/bottomEchogramPlot_T20190424_10291908-20190820_12575243.json
#C:/Data/S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json

spdf <- broom::tidy(geojsonio::geojson_read("C:/Data/StratumPolygon.geojson",  what = "sp"))

load("C:/Data/gps.Rdata")



School.dt <- makeSchoolDt(rawSv.dt = rawSv.dt, bottom.json = bottom.json, spdf = spdf, gps.dt = gps.dt)

assign(paste("School", "SD1032.dt", sep = "_"), School.dt)

save(School_EROS.dt, file = "C:/Data/School_data/School_EROS.Rdata")
save(School_SD1031.dt, file = "C:/Data/School_data/School_SD1031.Rdata")
save(School_SD1032.dt, file = "C:/Data/School_data/School_SD1032.Rdata")



