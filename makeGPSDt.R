rm(list=ls())
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
load("C:/Data/spdf.Rdata")
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


save(gps.dt, file="gps.Rdata")
rm(name, temp, gps_1.dt, text, label)


