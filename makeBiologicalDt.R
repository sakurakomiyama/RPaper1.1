rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())




#============================#
####   biological data    ####
#============================#

#== read data + marge with station data  ==#
lapply(c("C:/Data/stationIndividuals.Rdata", "C:/Data/spdf.Rdata"),load,.GlobalEnv)
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
#== combine with acoustic data ==#
sta.ind_2 <- sta.ind[!complete.cases(sta.ind[, c("latitudeend")]),] # without end lat-lon time 
sta.ind_1 <- sta.ind[complete.cases(sta.ind[, c("latitudeend")]),]  # with end lat-lon time

#== acoustic$id data.frame & trawl$serialnu,ber data.frame ==#
load("C:/Data/School_data/School_EROS.dt")
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



