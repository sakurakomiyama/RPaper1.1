rm(list=ls()) # remove all the variables from R environment
library(dplyr)
library(data.table)
library(ggplot2)
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())




#==============================#
####        gps data        ####
#==============================#

#== load original data ==#
library("rjson")
EROS.df <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019847_PEROS_3317/EXPORT/EchogramPlot_T20190423_16561743-20190512_19512565.json") #EROS
EROS.df <- data.table(PingNumber = EROS.df$pingNumber,  time=EROS.df$time,  Latitude = as.numeric(EROS.df$latitude), Longitude = as.numeric(EROS.df$longitude))
EROS.df$time <- as.POSIXct(EROS.df$time, origin = "1970-01-01", tz = "UTC")
library(lubridate)
EROS.df <- data.table(EROS.df)
EROS.df <- EROS.df[, lapply(.SD, mean), .(time = round_date(time, "10 seconds"))]
EROS.df$month <- strftime(EROS.df$time, "%m", tz="UTC")
EROS.df$YMD_time <- EROS.df$time

sd1032.df <- read.csv("C:/Users/a37907/Desktop/KnowSandeel15781/Data/CRUISE_LOG/sd-1032_GPS_20190424_20190820.csv" , header = TRUE, sep=",", dec=".")
sd1032.df$id <- 1
sd1032.df$area <- "area"
sd1032.df$YMD_time <- strptime(paste(sd1032.df$GPS_date, sd1032.df$GPS_time, sep=" "), "%Y-%m-%d %H:%M:%S", tz= "UTC") 
sd1032.df <- subset(sd1032.df, sd1032.df$Latitude!="na")
sd1032.df$month <- strftime(sd1032.df$YMD_time, "%m", tz="UTC")

gps <- read.csv("C:/Users/a37907/Desktop/KnowSandeel15781/Data/CRUISE_LOG/sd-1031_GPS_20190424_20190505.csv" , header = TRUE, sep=",", dec=".")
gps1 <- read.csv("C:/Users/a37907/Desktop/KnowSandeel15781/Data/CRUISE_LOG/sd-1031_GPS_20190510_20190626.csv" , header = TRUE, sep=",", dec=".")
gps2 <- read.csv("C:/Users/a37907/Desktop/KnowSandeel15781/Data/CRUISE_LOG/sd-1031_GPS_20190628_20190820.csv" , header = TRUE, sep=",", dec=".")
sd1031.df <- rbind (gps, gps1, gps2)
rm(gps,gps1,gps2)
sd1031.df$id <- 1
sd1031.df$area <- "area"
sd1031.df$YMD_time <- strptime(paste(sd1031.df$GPS_date, sd1031.df$GPS_time, sep=" "), "%Y-%m-%d %H:%M:%S", tz= "UTC") 
sd1031.df <- subset(sd1031.df, sd1031.df$Latitude!="na")
sd1031.df$month <- strftime(sd1031.df$YMD_time, "%m", tz="UTC")


#== load f.ground data, add names ==#
library("sf")
load("Data/spdf.Rdata")
test <- spdf %>% split(spdf$id) %>% 
  lapply(function(x) rbind(x,x[1,])) %>%
  lapply(function(x) x[,1:2]) %>%
  lapply(function(x) list(as.matrix(x))) %>%
  lapply(function(x) st_polygon(x))
points <- st_as_sf(EROS.df, coords=c('Longitude','Latitude'), remove = F)
polys <- test %>% st_sfc() %>% st_sf(geom=.) %>% mutate(id=factor(1:13)) 
temp <- polys  %>% st_intersection(points) 
temp <- mutate (temp, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "VestbankenSouthEast",
                                        id=="3" ~ "VestbankenSouthWest", id=="4" ~ "Vestbanken_North",
                                        id=="5" ~ "Vikingbanken", id=="6" ~ "Engelsk_Klondyke",
                                        id=="7" ~ "Inner_Shoal_East_2016", id=="8" ~ "Inner_Shoal_North",
                                        id=="9" ~ "Inner_Shoal_West_2018", id=="10" ~ "Inner_Shoal_test",
                                        id=="11" ~ "Nordgyden", id=="12" ~ "Ostbanken",
                                        TRUE ~ "Outer_Shoal"))

temp <- data.table(id = temp$YMD_time, area=temp$area)
temp <- temp[!duplicated(temp[,c('id')]),] # the first area will remain for data which are on a border of 2 areas 
temp2 <- data.table(id = EROS.df$YMD_time)
t <- merge(x = temp2, y = temp, by = "id", all.x = TRUE)
t$area[is.na(t$area)] = "outside"
EROS.df$area <- t$area
rm(test, points, polys, temp, temp2, t)


#== rbind "EROS.df" / "sd1031.df" / "sd1032.df" ==#
E <- data.table(id=EROS.df$PingNumber, Latitude=EROS.df$Latitude, Longitude=EROS.df$Longitude, YMD_time=EROS.df$time, area=EROS.df$area, month=EROS.df$month, vessel="EROS")
S1 <- data.table(id=sd1031.df$id, Latitude=sd1031.df$Latitude, Longitude=sd1031.df$Longitude, YMD_time=as.POSIXct(sd1031.df$YMD_time), area=sd1031.df$area, month=sd1031.df$month, vessel="SD1031")
S2 <- data.table(id=sd1032.df$id, Latitude=sd1032.df$Latitude, Longitude=sd1032.df$Longitude, YMD_time=as.POSIXct(sd1032.df$YMD_time), area=sd1032.df$area, month=sd1032.df$month, vessel="SD1032")
gps.dt <- rbind(E, S1, S2)
rm(EROS.df, sd1031.df, sd1032.df, E, S1, S2)


#== coverage ==#
gps.dt <- gps.dt[order(vessel, area, YMD_time),]
setDT(gps.dt)[, time_diff:=as.numeric(difftime(gps.dt$YMD_time, lag(gps.dt$YMD_time), units = "hours"))]
setDT(gps.dt)[, coverage:=as.numeric(1)]
gps.dt$coverage <- as.numeric(gps.dt$coverage)


# run by vessel
#========#
v = as.character("EROS") # "SD1031" / "SD1032" / "EROS"
tmp <- gps.dt[vessel %in% v] 

for (i in 2:nrow(tmp)){
  if (tmp$time_diff[i] >= 24)                                         #if time_diff is over 24 hours
    tmp$coverage[i] <- tmp$coverage[(i-1)] + 1                        #coverage number + 1
  else if (tmp$area[i] != tmp$area[(i-1)] &&                          #if time_diff is less than 24h and area[i] is not equal to area[i-1],
           nrow(filter(tmp[i:(i+49)], area == tmp$area[i]))>=40 &&    # and if area[i] continues more than 40 in the next 50 rows
           tmp$area[i] != tmp[coverage == coverage[i-1]]$area[1])     # and if area[i] is not equal to...
    tmp$coverage[i] <- tmp$coverage[(i-1)] + 1                        #coverage number + 1
  else
    tmp$coverage[i] <- tmp$coverage[(i-1)]                            #if not, coverage number[i] = coverage number [i-1]
}
#========#

tmp[, .(cnt= sum(.N)), by= c("coverage", "area")]

outside <- gps.dt[vessel%in%v & area%in%"outside"]
outside$coverage<- as.character(outside$coverage)
outside$coverage<- "F"

tmp <- data.table(rbind(tmp, outside)) #change data.table name depending on vessel "E":EROS, "s1":SD1031, "s2":SD1032
assign(paste("tmp", v, sep = "_"), tmp)


#== combine 3 data.table ==#
gps.dt <- data.table(rbind(tmp_EROS, tmp_SD1031, tmp_SD1032))
setDT(gps.dt)[, time_diff:=NULL]
rm(tmp_EROS, tmp_SD1031, tmp_SD1032, tmp, outside, x)

#== assign coverage name ==#
load("C:/Users/a37907/Desktop/KnowSandeel15781/Data/gps_1.Rdata")
load("Data/spdf.Rdata")
gps.dt <- gps_1.dt
# coverage names
setDT(gps.dt)[, coverage := ifelse(sum(.N)<=1000, paste(coverage,"x", sep = "."), coverage), by=c("coverage", "vessel")]
setDT(gps.dt)[, coverage_no := paste(vessel, coverage, sep = "-")]
name <- as.data.table(as.table(with(gps.dt, by(area, coverage_no, function(xx)names(which.max(table(xx)))))))
setDT(name)[, vessel := gsub('-.*',"",coverage_no)]
setDT(name)[, group_no := order(coverage_no), by  = c("N","vessel")]
setDT(name)[, coverage := ifelse(gsub('.*-',"",name$coverage_no) == ("F") , 
                                 coverage_no, paste(vessel, N, group_no, sep="-"))]
setDT(name)[, coverage := ifelse(endsWith(name$coverage_no, "x"),
                                  paste(coverage, "x", sep = "."), coverage)]
temp <- data.table(coverage_no = name$coverage_no, coverage_name = name$coverage)

#== merge with gps.dt ==#
gps.dt <- merge(x = gps.dt, y = temp, by = "coverage_no", all.x = TRUE)

#== coverage continuously traveled -> split into 2 by time ==#
setDT(gps.dt)[, coverage_name := ifelse(coverage_name == "SD1032-Engelsk_Klondyke-3", ifelse(YMD_time<=as.POSIXct('2019-06-14 23:59:59', tz="UTC"), "SD1032-Engelsk_Klondyke-3", "SD1032-Engelsk_Klondyke-4") ,coverage_name)]
setDT(gps.dt)[, coverage_name := ifelse(coverage_name == "SD1031-Vikingbanken-1", ifelse(YMD_time<=as.POSIXct('2019-06-04 08:43:00', tz="UTC"), "SD1031-Vikingbanken-1", "SD1031-Vikingbanken-2") ,coverage_name)]
setDT(gps.dt)[, coverage_name := ifelse(coverage_name %in% c("SD1031-Inner_Shoal_North-1", "SD1031-VestbankenSouthWest-1", "SD1031-VestbankenSouthWest-2"), "Transect", coverage_name)]

#== add start and end time ==#
setDT(gps.dt)[, StartTime:=min(YMD_time), by=(coverage_name)]
setDT(gps.dt)[, StopTime:=max(YMD_time), by=(coverage_name)]

#== add distance column ==#
library(geosphere)
gps.dt <- gps.dt[order(vessel,coverage_name, YMD_time),]
gps.dt$distance[2:nrow(gps.dt)] <- sapply(2:nrow(gps.dt), function(x) distm(gps.dt[x-1, c('Longitude', 'Latitude')], gps.dt[x, c('Longitude', 'Latitude')], fun = distHaversine))
setDT(gps.dt)[gps.dt[, .I[1], by=c("coverage_name","vessel")]$V1, distance:=0]
setDT(gps.dt)[, distance_sum:=sum(distance), by = c("coverage_name", "vessel")]

#== plot ==# (with start end time)
label <- data.table(aggregate(gps.dt$YMD_time ~ gps.dt$coverage_name + gps.dt$vessel, FUN = min), 
                    aggregate(gps.dt$YMD_time ~ gps.dt$coverage_name + gps.dt$vessel, FUN = max)[3])
colnames(label) <- c("coverage_name", "vessel", "start", "end")
label[gps.dt, on = 'coverage_name', area := area][gps.dt, on = 'coverage_name', distance_sum := distance_sum]
## "Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"
a <- c("VestbankenSouthWest")
v <- "EROS"
text <- label[label$area %like% a & label$vessel %in% v]
ggplot() + my_theme() + theme(axis.title = element_blank()) +
  geom_polygon(data= subset(spdf, area == a), aes(long,lat, group=area), col="black", alpha=.2) +
  geom_point(data=gps.dt[area %in% a & vessel %in% v], 
             aes(x = Longitude, y = Latitude), size=.05, alpha =0.2) +
  geom_text(data = text, aes(x = -Inf, y = Inf, label = start), hjust   = 0, vjust   = 1) + 
  geom_text(data = text, aes(x = -Inf, y = -Inf, label = end), hjust   = 0, vjust   = 0) + 
  #geom_text(data = text, aes(x = -Inf, y = Inf, label = round(distance_sum, digits = 0)), hjust   = 0.7, vjust   = 0) + 
  facet_wrap(~coverage_name, scales="free")
gps.dt[, .(cnt= sum(.N)), by= c("vessel", "coverage_name")]

#save(EROS.df, file="EROS.Rdata")
#save(sd1031.df, file="sd1031.Rdata")
#save(sd1032.df, file="sd1032.Rdata")
save(gps.dt, file="gps.Rdata")
rm(name, temp, gps_1.dt, text, label)











#===============================#
####    environment data     ####
#===============================#


library(ncdf4)

#== load original data ==#
files <- list.files('S2019_SAILDRONE_1032/PHYSICS/DAILY_FILES/',pattern='*.nc', full.names=TRUE) ## 'S2019_SAILDRONE_1031/PHYSICS/DAILY_FILES/'
temp_df = data.frame(matrix(nrow=1))                                ## create empty data frame with 1row
env.dt <- data.table()
#env.dt = data.frame(matrix(rep(NA, 61), nrow=1))[numeric(0), ]   ## create empty data frame with 61columns

for(i in 1:length(files)) {                                         ## loop for all files
  temp_nc <- nc_open(files[i])                                      ## open NetCDF(.nc) file and put it into "temp_nc"
  for (x in 1:temp_nc$nvars) {                                      ## loop for all variables (60 variables)
    var <-  ncvar_get(temp_nc, temp_nc$var[[x]])                    ## extract variables from NetCDF file
    temp_df <- cbind(temp_df, var)                                  ## add to empty data fram
    names(temp_df)[length(names(temp_df))] <- 
      ifelse(substr(names(temp_nc$var[x]), nchar(names(temp_nc$var[x]))-1, nchar(names(temp_nc$var[x])))=="SD", 
             gsub("_SD", "_STDDEV",names(temp_nc$var[x])),
             names(temp_nc$var[x]))                                 ## rename the last column eg)"var" -> "time"
    temp_df <- temp_df[,!grepl("WING_", colnames(temp_df))]
  }
  env.dt <- rbind(env.dt, temp_df, fill=TRUE)                       ## merge temp_df to env.dt
  temp_df = data.frame(matrix(nrow=1))                              ## reset temp_df to empty data frame
  nc_close(temp_nc)                                                 ## close the temp_nc
}

rm(temp_df, temp_nc, files, i, var, x)


#== convert time ==#
env.dt$matrix.nrow...1. <- env.dt$time / 86400 + 25569
env.dt$matrix.nrow...1. <- as.POSIXct(env.dt$time, tz="UTC", origin="1970-01-01")
names(env.dt)[names(env.dt) == "matrix.nrow...1."] <- "YMD_time"

#== add wind speed & direction column ==#
env.dt$WND_speed <- sqrt(env.dt$UWND_MEAN^2 + env.dt$VWND_MEAN^2)
env.dt$WND_direction <- atan2(env.dt$UWND_MEAN, env.dt$VWND_MEAN)*180/pi+180
## To convert from "math direction" to "meteorological direction", use use this formula (in degrees)
## meteorological direction = 270 ??? math direction

#== convert NAN -> NA (for calculating e.g. mean)  ==#
is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))
env.dt[is.nan(env.dt)] <- NA

#== add vessel column & save ==#
#env_1031.dt <- env.dt
#env_1031.dt$vessel <- "SD1031"
#* run the code again for SD1032 and combine *#
env_1032.dt <- env.dt
env_1032.dt$vessel <- "SD1032"
env.dt <- rbind(env_1031.dt,env_1032.dt)
save(env.dt, file="env.Rdata")


#== EROS CTD ==#
ctd_EROS.dt <- data.table()
filename <- list.files('S2019847_PEROS_3317/PHYSICS/CTD/CTD_DATA/CNVmHDR',pattern='*.cnv', full.names=TRUE)
for(i in 1:length(filename)){
  temp <- readLines(filename[i])
  time <- gsub(".*= ","",temp[grep('start_time = ',temp)])
  time <- gsub('\\[.*',"",time)
  lat <- gsub(".*= ","",temp[2])
  lon <- gsub(".*= ","",temp[3])
  sta <- gsub(".*: ","",temp[4])
  nlines.START <- grep('name 0', temp)-1
  nlines.END <- grep('= flag:', temp)
  nlines.variables <- nlines.END-nlines.START
  v1 <- scan(filename[i], what='raw', skip=nlines.START, nlines=nlines.variables, sep='\n')
  v1 <- gsub(".*:","",v1)
  v1 <- gsub(",.*","",v1)
  v1 <- gsub('\\[.*',"",v1)
  nlines <- grep('*END*', temp)
  temp <-read.table(filename[i], skip=nlines, header=F, sep="")
  names(temp) <- v1
  setDT(temp)[, start_time:=time][, Latitude:=lat][, Longitude:=lon][,station:=sta]
  ctd_EROS.dt <- data.table(rbind(ctd_EROS.dt,temp))
}
rm(nlines, nlines.START, nlines.END, nlines.variables, v1, time, sta, lat, lon)

library(stringr)
ctd_EROS.dt$start_time <- str_replace_all(ctd_EROS.dt$start_time, c("Apr" = "04", "May" = "05"))
ctd_EROS.dt$start_time <- gsub(" 2019 ","", ctd_EROS.dt$start_time)
ctd_EROS.dt$start_time <- paste("2019", ctd_EROS.dt$start_time, sep = " ")
ctd_EROS.dt$start_time <- gsub(" ","-", ctd_EROS.dt$start_time)
ctd_EROS.dt$start_time <- sub("\\s+$", "", gsub('(.{10})', '\\1 ', ctd_EROS.dt$start_time))
ctd_EROS.dt$start_time <- ifelse(stringr::str_sub(ctd_EROS.dt$start_time,-1,-1)=="-",substr(ctd_EROS.dt$start_time,1,nchar(ctd_EROS.dt$start_time)-1),ctd_EROS.dt$start_time)
ctd_EROS.dt$start_time <- as.POSIXct(ctd_EROS.dt$start_time, "%Y-%m-%d %H:%M:%S", tz= "UTC")

ctd_EROS.dt$Latitude <- as.numeric(substr(ctd_EROS.dt$Latitude, 1,2)) + (as.numeric(substr(ctd_EROS.dt$Latitude, 4,5))/60) + (as.numeric(substr(ctd_EROS.dt$Latitude, 7,8))/3600)
ctd_EROS.dt$Longitude <- as.numeric(substr(ctd_EROS.dt$Longitude, 1,3)) + (as.numeric(substr(ctd_EROS.dt$Longitude, 5,6))/60) + (as.numeric(substr(ctd_EROS.dt$Longitude, 8,9))/3600)
save(ctd_EROS.dt, file="ctd_EROS.Rdata")
#
#




















#============================#
####   biological data    ####
#============================#

#== read data + marge with station data  ==#
lapply(c("Data/stationIndividuals.Rdata", "Data/spdf.Rdata"),load,.GlobalEnv)
sta.ind <- Filter(function(x)!all(is.na(x)), sta.ind)
#== time ==#
library(stringr)
setDT(sta.ind)[,date := as.POSIXct(date,"%Y-%m-%d %H:%M:%S", tz= "UTC")]
sta.ind <- sta.ind[date >= as.POSIXct('2019-04-24 00:00:00', tz="UTC") & date <= as.POSIXct('2019-05-12 23:59:59', tz="UTC")]
setDT(sta.ind)[,Time_start:= ifelse(is.na(stationstarttime), paste(date, "12:00", sep=" "), paste(date, stationstarttime, sep=" "))]
setDT(sta.ind)[,Time_start:=as.POSIXct(sta.ind$Time_start, "%Y-%m-%d %H:%M", tz= "UTC")]
setDT(sta.ind)[,Time_end:=ifelse(is.na(stationstoptime), paste(Time_start), paste(date, stationstoptime, sep=" "))]
setDT(sta.ind)[,Time_end:=as.POSIXct(sta.ind$Time_end, "%Y-%m-%d %H:%M", tz= "UTC")]
setDT(sta.ind)[,Longitude:= ifelse(is.na(longitudeend), longitudestart, (longitudeend+longitudestart)/2)][, Latitude := ifelse(is.na(latitudeend), latitudestart, (latitudeend+latitudestart)/2)]
sta.ind$YMD_time <- sta.ind$date # for plot facet

#=== classify to the areas ===#
library("sf")
test <- spdf %>% split(spdf$id) %>% 
  lapply(function(x) rbind(x,x[1,])) %>%
  lapply(function(x) x[,1:2]) %>%
  lapply(function(x) list(as.matrix(x))) %>%
  lapply(function(x) st_polygon(x))
points <- st_as_sf(sta.ind,coords=c('Longitude','Latitude'),remove = F)
polys <- test %>% st_sfc() %>% st_sf(geom=.) %>% mutate(id=factor(1:13)) 
temp <- polys  %>% st_intersection(points) 
temp <- mutate (temp, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "VestbankenSouthEast",
                                        id=="3" ~ "VestbankenSouthWest", id=="4" ~ "Vestbanken_North",
                                        id=="5" ~ "Vikingbanken", id=="6" ~ "Engelsk_Klondyke",
                                        id=="7" ~ "Inner_Shoal_East_2016", id=="8" ~ "Inner_Shoal_North",
                                        id=="9" ~ "Inner_Shoal_West_2018", id=="10" ~ "Inner_Shoal_test",
                                        id=="11" ~ "Nordgyden", id=="12" ~ "Ostbanken",
                                        TRUE ~ "Outer_Shoal"))


temp <- data.table(serialnumber=temp$serialnumber, area=temp$area)
temp <- temp[!duplicated(temp[,c('serialnumber')]),] # the first area will be remain for data which are on a border of 2 areas 
temp2 <- data.table(serialnumber=sta.ind$serialnumber)
t <- merge(x = temp2, y = temp, by = "serialnumber", all.x = TRUE)
t[is.na(t$area)] = "outside"
sta.ind$area <- t$area
rm(test, points, polys, temp, temp2, t)



#== plot ==#
ggplot(data=sta.ind, aes(x=LengthCentimeter)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_histogram(binwidth=1, color="black", fill="white") + 
  facet_wrap(~area, scale = "free_y") + 
  labs(x="length (cm)", y="Number")

ggplot(data=sta.ind, aes(x=area, y=LengthCentimeter)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + 
  geom_boxplot(color="black", fill="white") + labs(y="length (cm)")

summary(lm(length ~  area, data=sta.ind))

ggplot() + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(data=School_EROS.dt, aes(x=Longitude, y=Latitude), col="red") + 
  geom_point(data=sta.ind, aes(x=Longitude, y=Latitude), col="blue") + 
  facet_wrap(~strftime(YMD_time, format="%m%d", tz="UTC"))+ 
  labs(x="Longitude",y="Latitude")
#

#
#== combine with "School:rf.dt" ==#
sta.ind_2 <- sta.ind[!complete.cases(sta.ind[, c("latitudeend")]),] # without end lat-lon time 
sta.ind_1 <- sta.ind[complete.cases(sta.ind[, c("latitudeend")]),]  # with end lat-lon time
#== acoustic$id data.frame & trawl$serialnu,ber data.frame ==#
acoustic <- subset(School_EROS.dt, category %in% c("SAND", "PSAND"))
acoustic <- unique(data.table(id=acoustic$id, category=acoustic$category, Latitude = acoustic$Latitude, Longitude=acoustic$Longitude, YMD_time=acoustic$YMD_time))
trawl_1 <- unique(data.table(serialnumber=sta.ind_1$serialnumber, Latitudestart=sta.ind_1$latitudestart, Latitudeend=sta.ind_1$latitudeend,Longitudestart=sta.ind_1$longitudestart, Longitudeend=sta.ind_1$longitudeend, Time_start=sta.ind_1$Time_start, Time_end=sta.ind_1$Time_end))
trawl_2 <- unique(data.table(serialnumber=sta.ind_2$serialnumber, Latitudestart=sta.ind_2$latitudestart,Longitudestart=sta.ind_2$longitudestart, Time_start=sta.ind_2$Time_start, Time_end=sta.ind_2$Time_end))
#= min and max of Lat Long =#
setDT(trawl_1)[,Lat := Latitudeend-Latitudestart][,Latitudemin:= ifelse(Lat > 0, Latitudestart, Latitudeend)][,Latitudemax:= ifelse(Lat < 0, Latitudestart, Latitudeend)][, Lat:=NULL]
setDT(trawl_1)[,Lon := Longitudeend-Longitudestart][,Longitudemin:= ifelse(Lon > 0, Longitudestart, Longitudeend)][,Longitudemax:= ifelse(Lon < 0, Longitudestart, Longitudeend)][, Lon:=NULL]
setDT(trawl_2)[,Latitudemin := Latitudestart - 0.1][,Latitudemax:= Latitudestart + 0.1][,Longitudemin:=Longitudestart-0.1][,Longitudemax:=Longitudestart+0.1] # arbitrary range 0.1 degree
setDT(trawl_2)[,Time_end2 :=ifelse(Time_start!=Time_end, Time_end, Time_start + 5*60*60)][,Time_end2 := as.POSIXct(Time_end2, origin='1970-01-01', tz="UTC")] # mean Time difference of 5.060526 hours
#= find acoustic schools within trawl operation (position min~max) =#
setDT(acoustic)[setDT(trawl_1), on =. (Latitude>=Latitudemin, Latitude<=Latitudemax, Longitude>=Longitudemin, Longitude<=Longitudemax, YMD_time>=Time_start, YMD_time<=Time_end), serialnumber := serialnumber]
setDT(acoustic)[setDT(trawl_2), on =. (Latitude>=Latitudemin, Latitude<=Latitudemax, Longitude>=Longitudemin, Longitude<=Longitudemax, YMD_time>=Time_start, YMD_time<=Time_end2), serialnumber2 := serialnumber]
#== merge data ==#
acoustic <- acoustic[complete.cases(acoustic[ , 6]),] # 81 schools are in 22 trawl station 
acoustic <- School_EROS.dt[acoustic, on = "id", roll = TRUE]
acoustic <- subset(acoustic, select=-c(i.Latitude,i.Longitude, i.YMD_time))
School_EROS_bio.df <- sta.ind[acoustic, on = "serialnumber", roll = TRUE, allow.cartesian=TRUE]
setDT(School_EROS_bio.df)[, Length_n := .N, by = c("LengthCentimeter", "Frequency", "serialnumber")][, weighted_meanLength := weighted.mean(LengthCentimeter, Length_n), by = c("serialnumber", "Frequency")]
setDT(School_EROS_bio.df)[, meanLength := mean(LengthCentimeter), by = c("serialnumber", "Frequency")]
setDT(School_EROS_bio.df)[, id_n := .N, by = c("specimenid", "Frequency", "serialnumber")]
rm(acoustic, trawl_1, trawl_2, sta.ind_1, sta.ind_2)
#

save(School_EROS_bio.df, file="School_EROS_bio.df")



#== plot ==#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% c(200)), aes(x=serialnumber, y=id)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_jitter(size=0.1)+
  labs(x="trawl station",y="school id")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% c(200)), aes(x=serialnumber, y=id_n)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x  = element_blank())+
  geom_point(shape=1) + scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  labs(x="trawl station",y="number of acoustic schools")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% c(200)), aes(y=id, x=LengthCentimeter)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point()+
  labs(y="school id",x="body length (cm)")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% c(200)), aes(x=weighted_meanLength, y=rf)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(shape=1)+
  labs(x="mean body length (cm)",y="200kHz frequency response")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% 200), aes(x=LengthCentimeter)) + theme_bw(base_size=10) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_histogram(col="black",fill="white") +
  #facet_wrap(~id) + 
  labs(x="body length (cm)",y="number of fish")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% 200), aes(x=LengthCentimeter, y=rf)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(shape=1)+
  stat_summary(fun=mean, colour="red", geom="point", shape=1)+stat_summary(fun=mean, colour="red", geom="line")+
  labs(x="body length (cm)",y="200kHz frequency response")
#
ggplot(data=subset(School_EROS_bio.df, Frequency %in% 200), aes(x=LengthCentimeter, y=school_area)) + theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(shape=1) + 
  stat_summary(fun=mean, colour="red", geom="point", shape=1)+stat_summary(fun=mean, colour="red", geom="line")+
  labs(x="body length (cm)",y="school size")
#





#=============================#
####  bottom depth data    ####
#=============================#
library("rjson")
#bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019847_PEROS_3317/EXPORT/bottomEchogramPlot_T20190423_16561743-20190512_19512565.json") #EROS
#bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json") #SD1032  
bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1031/EXPORT/bottomEchogramPlot_T20190424_10291908-20190820_12575243.json")  #SD1031

#== Repeat 3 times for each vessel ==#
bottom.dt <- data.table(PingNumber = bottom.json$pingNumber, 
                        BottomDepth = as.numeric(bottom.json$lowerLayerBoundary), 
                        PingDistance = as.numeric(bottom.json$vesselDistance), 
                        time = as.numeric(bottom.json$time))
setDT(bottom.dt)[, Diff:=c(0, diff(bottom.dt$PingDistance))][, distance:= Diff*1852][,Diff:=NULL][,PingDistance:=NULL]
bottom.dt$vessel <- as.character("SD1031") # "EROS" / "SD1031" / "SD1032" 
bottom_EROS <- bottom.dt # "bottom_EROS" / "bottom_SD1031" / "bottom_SD1032"
#====================================#

bottom.dt <- rbind(bottom_EROS, bottom_SD1031, bottom_SD1032)
save(bottom.dt, file="C:/Users/a37907/Desktop/bottom.Rdata")




























































































































































































































































#========================================================================================================================#
#### test code    ####  
#========================================================================================================================#

#replace NaN by interpolating previous and next non-NaN observation values
#library("zoo")
#bottom.dt$BottomDepth <- zoo::na.approx(bottom.dt$BottomDepth) 




#temp_dt = data.table(matrix(nrow=1))                                
#temp_dt = data.table(matrix(rep(NA, 1), nrow=1))[numeric(0), ]   ## create empty data table with 61columns
#SvSchool.dt = data.table(matrix(rep(NA, 1), nrow=1))[numeric(0), ]   ## create empty data table with 61columns

#== create empty data table
temp_dt <- data.table()   
SvSchool.dt <- data.table()
#== This pattern will be used in loop when extracting school ID from file name
#== pattern : extract strings between "SvSchool" and "_T2019"
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"

#== Read file name and directory of text data
#files <- list.files('S2019847_PEROS_3317/EXPORT/Sv_SAND_2.11.0-rc1',pattern='txt$', full.names=TRUE)
files <- list.files('S2019847_PEROS_3317/EXPORT/test', pattern='txt$', full.names=TRUE)

#== read file(i) and process row by row(r)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "SAND", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  
  for (r in 1:nrow(temp)){
    range_bin <- (temp$RangeStop[r]-temp$RangeStart[r])/temp$SampleCount[r]   ## calculate range bin
    depth_bin <- (temp$DepthStop[r]-temp$DepthStart[r])/temp$SampleCount[r]   ## calculate depth bin
    
    for (s in 1:temp$SampleCount[r]) {
      Sv <- as.character(paste0("Sv", s))                                     ## create char "Sv1","Sv2","Sv3"...depends on (r)
      temp_dt <- cbind (temp[r,1:13])                                         ## fetch column 1:13 of text data and insert to the empty datatable
      temp_dt$sampleNo <- Sv                                                  ## add column that tells "Sv1","Sv2","Sv3"...
      temp_dt$rangeStart <- temp$RangeStart[r] + (range_bin * (s-1))          ## calculate and input range start of "Sv1"
      temp_dt$rangeStop <- temp_dt$rangeStart + range_bin                     ## calculate and input range stop of "Sv1"
      temp_dt$depthStart <- temp$DepthStart[r] + (depth_bin * (s-1))          ## calculate and input depth start of "Sv1"
      temp_dt$depthStop <- temp_dt$depthStart + depth_bin                     ## calculate and input depth stop of "Sv1"
      temp_dt$Sv <- as.numeric(temp[r,Sv,with=FALSE])                         ## add column of sV values of row(r) and sampleNo(s)
      SvSchool.dt <- rbind(SvSchool.dt, temp_dt,fill=TRUE)                    ## combine to Svschool.dt
    }
    SvSchool.dt <- SvSchool.dt[!is.na(SvSchool.dt$Sv), ]                      ## remove rows if sV value = NA
    
  }
  
}

# check id number
for(i in 1:length(files)) {
  temp <- rbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp, fill=TRUE)
}
#
write.csv(temp, file="1031_noise.csv")


SvSchool.dt <- 
  SvSchool.dt %>%
  group_by(id, PingNumber, SampleNo) %>% 
  mutate(rf=Sv[Frequency == 38]/Sv)


SvMean.dt <- as.data.table(aggregate(SvSchool.dt$Sv, FUN=mean, by=list(id=SvSchool.dt$id,frequency=SvSchool.dt$Frequency)))
SvMean.dt <- SvMean.dt[order(id),]
SvMean.dt <- 
  SvMean.dt %>% 
  group_by(id, sampleNo) %>% 
  mutate(rf=x[frequency == 38]/x)


#== Distance between pings ==#
dist <- data.table(id= SvSchool_1032.dt$id, PingNumber=SvSchool_1032.dt$PingNumber, YMD_time=SvSchool_1032.dt$YMD_time, Latitude=SvSchool_1032.dt$Latitude, Longitude=SvSchool_1032.dt$Longitude)
dist <- unique(dist, by = c("PingNumber"))
dist <- dist[order(PingNumber),]
library(geosphere)
dist$distance[2:nrow(dist)] <- sapply(2:nrow(dist), function(x) distm(dist[x-1, c('Longitude', 'Latitude')], dist[x, c('Longitude', 'Latitude')], fun = distHaversine))
dist[, distance := replace(distance, 1, distance[2]), by = id]
colSums(is.na(dist))
dist[is.na(dist)] <- mean(dist$distance, na.rm = TRUE) #1 ping school will have mean(distance of all ping distance)
colSums(is.na(dist))
dist <- data.table(PingNumber=dist$PingNumber, distance = dist$distance)
#== merge distance with SvSchool.dt ==#
SvSchool_1032.dt <- merge(x = SvSchool_1032.dt, y = dist, by = "PingNumber", all.x = TRUE)
setDT(SvSchool_1032.dt)[, pixel_size := distance*depth_bin]
setDT(SvSchool_1032.dt)[, school_area := sum(pixel_size), by = c("id", "Frequency")]
rm(dist)
#== School horizontal length ==#
dist <- data.table(id= SvSchool_1032.dt$id, PingNumber=SvSchool_1032.dt$PingNumber, Latitude=SvSchool_1032.dt$Latitude, Longitude=SvSchool_1032.dt$Longitude)
dist <- unique(dist, by = c("PingNumber"))
dist_1 <- setDT(dist)[, head(.SD,1), by = id]
dist_2 <- setDT(dist)[, tail(.SD,1), by = id]
dist <- rbind(dist_1, dist_2)
dist <- dist[order(PingNumber),]
library(geosphere)
dist$distance[2:nrow(dist)] <- sapply(2:nrow(dist), function(x) distm(dist[x-1, c('Longitude', 'Latitude')], dist[x, c('Longitude', 'Latitude')], fun = distHaversine))
dist <- setDT(dist)[, tail(.SD,1), by = id]
dist <- data.table(id=dist$id, school_length = dist$distance)
SvSchool_1032.dt <- merge(x = SvSchool_1032.dt, y = dist, by = "id", all.x = TRUE)
rm(dist, dist_1, dist_2)
#

#== coverage number ==#
School.dt <- School.dt[order(vessel, area, Frequency, YMD_time),]
School.dt <- School.dt[order(vessel, area, YMD_time),]
setDT(School.dt)[, time_diff:=as.numeric(difftime(School.dt$YMD_time, lag(School.dt$YMD_time), units = "hours"))]
setDT(School.dt)[, coverage:=as.numeric(1)]
School.dt$coverage <- as.numeric(School.dt$coverage)

for (i in 2:nrow(School.dt)){
  if (School.dt$area[i]!="outside" && School.dt$time_diff[i]>=24)
    School.dt$coverage[i] <- School.dt$coverage[(i-1)] + 1
  
  else if (School.dt$area[i]!="outside" && School.dt$area[i]!=School.dt$area[(i-1)] && 
           nrow(filter(School.dt[i:(i+49)], area==School.dt$area[i]))>=40 && 
           School.dt$area[i]!= School.dt[coverage==coverage[i-1]]$area[1])
    
    School.dt$coverage[i] <- School.dt$coverage[(i-1)] + 1
  
  else if (School.dt$area[i]=="outside")
    School.dt$coverage[i] <- "F"
  else
    School.dt$coverage[i] <- School.dt$coverage[(i-1)]
  
}

School.dt[, .(cnt= sum(.N)), by= c("coverage", "area")]
ggplot() +theme_bw(base_size=15) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text = element_text(size=5,), axis.ticks.length  = unit(1, "mm"), strip.text = element_text(size = 7))+geom_path(data = School.dt[Frequency%in%200], aes(Longitude, Latitude, colour=as.factor(coverage)))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + facet_wrap(~coverage, scales="free")
setDT(School.dt)[, time_diff:=NULL]



ggplot(data=subset(SvSchool.dt, Frequency %in% c(18, 38, 70,120, 200)), aes(x=Frequency, y = rf)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_point(data=subset(SvSchool.dt, Frequency %in% c(38, 200)), aes(x=Frequency, y = rf )) + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  stat_summary(fun = mean, geom="point") + stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, fun.min = function(x) mean(x) - sd(x), fun.max = function(x) mean(x) + sd(x), geom = "errorbar", width = 7) +
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) + scale_y_continuous(limits = c(0, 2.5)) + #, breaks=c(0.5, 1.0, 1.5)
  facet_grid(category~area)+
  labs(x="Frequency", y="Frequency response")
#
ggplot(data=subset(SvSchool.dt, id %in% c(5681)), aes(x=Frequency, y = rf )) + #colour=id, group=id
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_jitter(col="gray", size=0.5, shape=1,stroke = 1 ) + 
  geom_point() + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  #  geom_point(data=subset(SvSchool.dt, Frequency %in% c(38, 200)), aes(x=Frequency, y = rf )) + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) + # scale_y_continuous(limits = c(0.5, 1.5), breaks=c(0.5, 1.0, 1.5)) +
  #  facet_wrap(~id, scale="free_y") +
  facet_grid(.~category)+
  labs(x="Frequency", y="Frequency response")
#



##== check Sv value and frequency response of one school ==##
temp.dt <- as.data.table(read.table('S2019_SAILDRONE_1032/EXPORT/notuse_PSAND/SvSchool1615_T20190430_00595792-20190819_18193333_01.txt', header = TRUE, sep = ","))
temp.dt <- melt(temp.dt, id = 1:11)
temp.dt <- temp.dt[!is.na(temp.dt$value), ]
setnames(temp.dt, c("variable", "value"), c("SampleNo","Sv"))
temp.dt$depth_bin <- (temp.dt$DepthStop- temp.dt$DepthStart)/temp.dt$SampleCount
temp.dt$depthStart <- temp.dt$DepthStart + (temp.dt$depth_bin * (temp.dt$SampleCount-1))
temp.dt$depthStop <- temp.dt$depthStart + temp.dt$depth_bin

setDT(temp.dt)[, sV := 10^(Sv/10)][, sV38 := sV[Frequency==38], by = c("PingNumber", "SampleNo")][, sV_mean := mean(sV), by = c("Frequency")][, rf := sV_mean/sV_mean[Frequency==38]]
setDT(temp.dt)[, PingNo := max(PingNumber)-min(PingNumber) + 1][, sV_stdv := sd(sV), by = c("Frequency")][, SE_f := sV_stdv/sqrt(PingNo*SampleCount), by = c("Frequency")][, SE := SE_f/mean(sV38), by = c("Frequency")]

#plot(data=temp.dt, rf~Frequency)
#aggregate(rf ~ Frequency, data=temp.dt, mean)

axis <- data.frame(y1=c(min(temp.dt$Sv), max(temp.dt$Sv)), y2=c(min(temp.dt$rf), max(temp.dt[temp.dt$Frequency!=333,]$rf)))
A2D_summary <- summary(lm(formula = y1 ~ y2, data = axis)) 
A2DInt <- A2D_summary$coefficients[1, 1] 
A2DSlope <- A2D_summary$coefficients[2, 1]
D2A_summary <- summary(lm(formula = y2 ~ y1, data = axis)) 
D2AInt <- D2A_summary$coefficients[1, 1]
D2ASlope <- D2A_summary$coefficients[2, 1]
ggplot(data=subset(temp.dt,Frequency%in%c(18,38,70,120,200)),aes(x=Frequency, y=Sv) ) + geom_jitter(shape=1, size=1) + 
  geom_point(aes(y=rf*A2DSlope+A2DInt), col="red") + geom_line(aes(y=rf*A2DSlope+A2DInt), col="red") + scale_y_continuous("SV", sec.axis = sec_axis(~.*D2ASlope+D2AInt, name="Frequency response"),)+
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) +  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
#
rm(axis, A2D_summary, A2DInt, A2DSlope, D2A_summary, D2AInt, D2ASlope)





#== check each polygon ==#
#library("sp")
#point.in.polygon(test$x,test$y,polygon_EK$longitude,polygon_EK$latitude)
file1 <- "StratumPolygon.geojson"
library(geojsonio)
spdf <- geojson_read(file1,  what = "sp")
#plot(spdf, col="grey")
#
library(broom)
spdf_fortified <- tidy(spdf)
spdf_fortified <- mutate (spdf_fortified, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                                            id=="3" ~ "Inner_Shoal_East_2016", id=="4" ~ "Inner_Shoal_North",
                                                            id=="5" ~ "Inner_Shoal_West_2018", id=="6" ~ "Inner_Shoal_test",
                                                            id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                                            id=="9" ~ "Outer_Shoal", id=="10" ~ "VestbankenSouthEast",
                                                            id=="11" ~ "VestbankenSouthWest", id=="12" ~ "Vestbanken_North",
                                                            TRUE ~ "Vikingbanken"))
spdf_fortified <- mutate (spdf_fortified, area2 = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                                             id=="3" ~ "Inner_Shoal", id=="4" ~ "Inner_Shoal",
                                                             id=="5" ~ "Inner_Shoal", id=="6" ~ "Inner_Shoal",
                                                             id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                                             id=="9" ~ "Outer_Shoal", id=="10" ~ "Vestbanken",
                                                             id=="11" ~ "Vestbanken", id=="12" ~ "Vestbanken",
                                                             TRUE ~ "Vikingbanken"))


#==biological data from English Klondyke ==#
setwd("C:/Users/komiy/Desktop/jugyo/BIO399_MSc/Data")
library(data.table)
load("Biological_data/EngKlondyke2019_biodata.Rdata")
# Load the attached file: conatin station, catch & individual info.
sta.ind <- merge(sta2,ind1)
sta.ind$week <- week(sta.ind$date)
# Some output
len.dist.week <- tapply(sta.ind$length, list(sta.ind$length, sta.ind$week), length)
boxplot(sta.ind$length ~sta.ind$week, xlab="Week number", ylab="Length (cm)")

#== calculate distance from lat lon ==#
gridDim <- acos(sin(pi*(min(test$LATITUDE))/180.0)*sin(pi*(max(test$LATITUDE))/180.0)+
                  cos(pi*(min(test$LATITUDE))/180.0)*cos(pi*(max(test$LATITUDE))/180.0)*
                  cos(pi*(min(test$LONGITUD))/180.0-pi*(max(test$LONGITUD))/180.0))*6378


#   error txt.file from 1032
notuse_school.dt <- data.table(longitude=c(3.911060,3.991163,3.987633,3.979058,4.013532,3.992762,3.992107,
                                           3.991143,3.991148,3.991693,3.991150,3.991385,3.907003,3.907187,3.907920,
                                           3.927997,3.967090,3.906043,3.902615,3.902937,3.921058,3.919362,3.912782,
                                           3.972690,3.918762,3.917150,3.916098,3.914552,3.911685,3.912162), 
                               latitude=c(56.770847,56.750652,56.755875,56.760690,56.668268,56.736295,56.739108,
                                          56.749097,56.748453,56.743565,56.750112,56.745763,56.736165,56.739842,56.739290,
                                          56.807508,56.811503,56.735167,56.731685,56.732008,56.787138,56.777103,56.773018,
                                          56.783767,56.765828,56.763973,56.762858,56.761250,56.757800,56.758347),
                               id=c(1615,3355,3358,3359,3360,3363,3364,3368,3369,3370,3371,3372,3382,3384,3385,3386,
                                    3391,3392,3393,3394,3400,3403,3404,3407,3408,3409,3410,3411,3412,3413))
pos <- data.frame(id=data$id, Latitude=data$Latitude, Longitude=data$Longitude)
library("ggthemes") # for theme_map
w <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data = w) + geom_sf() + theme_bw(base_size=15) +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  coord_sf(xlim = c(0, 8), ylim = c(56, 61), expand = FALSE)+ #, datum = NA
  annotate(geom = "text", x = 7, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 10)+
  geom_polygon(data = spdf, aes(long, lat, group=area, fill=area), colour="black", alpha =0.3) + 
  #  geom_point(data=fishstation.dt, aes(x=longitudestart, y=latitudestart), size=1, shape=1, col="blue")
  geom_point(data=notuse_school.dt, aes(x=longitude, y=latitude), size=.8, shape=1)+
  labs(x="Longitude", y="Latitude")



#============================#
###    biological data     ###
#============================#
#== read data + marge with station data  ==#
bio.dt <- as.data.table(read.table("S2019847_PEROS_3317/BIOLOGY/CATCH_MEASUREMENTS/BIOTIC/individual.txt", header = TRUE, sep = "\t"))
fishstation.dt <- as.data.table(read.table("S2019847_PEROS_3317/BIOLOGY/CATCH_MEASUREMENTS/BIOTIC/fishstation.txt", header = TRUE, sep = "\t"))
setnames(fishstation.dt, c("serialnumber"), c("f.serialnumber"))

bio.dt <- fishstation.dt[bio.dt, on = "f.serialnumber", roll = TRUE]
bio.dt <- Filter(function(x)!all(is.na(x)), bio.dt)
bio.dt <- bio.dt[, !duplicated(as.list(bio.dt)), with = FALSE]
setDT(bio.dt)[, Longitude := (longitudeend+longitudestart)/2][, Latitude := (latitudeend+latitudestart)/2]
setDT(bio.dt)[,length_cm := (ifelse(lengthresolution==2, length*100, length*10))]


#== time ==#
#library(stringr)
#setDT(sta.ind_1)[,mo:=stringr::str_sub(sta.ind_1$stationstartdate,6,7)][, da:=stringr::str_sub(sta.ind_1$stationstartdate,9,10)][,Time_start:=paste(startyear, mo, da, sep="-")]
#setDT(sta.ind_1)[,Time_start:=paste(Time_start, stationstarttime, sep=" ")]
#setDT(sta.ind_1)[,Time_start:=as.POSIXct(sta.ind_1$Time_start, "%Y-%m-%d %H:%M", tz= "UTC")]
#setDT(sta.ind_1)[,Time_end:=paste(startyear, mo, da, sep="-")][,Time_end:=paste(Time_end, stationstoptime, sep=" ")]
#setDT(sta.ind_1)[,Time_end:=as.POSIXct(sta.ind_1$Time_end, "%Y-%m-%d %H:%M", tz= "UTC")][,mo:=NULL][,da:=NULL]











#== center of gravity ==#
center_of_mass <- function(x,y,w){
  c(crossprod(x,w)/sum(w), crossprod(y,w)/sum(w))}
center_of_mass(data$Latitude, data$Longitude, data$sA)



#acos(sin(pi*(Latitude)/180.0)*sin(pi*(Latitudestart)/180.0)+cos(pi*(Latitude)/180.0)*cos(pi*(Latitudestart)/180.0)*cos(pi*(Longitude)/180.0-pi*(Longitudestart)/180.0))*6378
