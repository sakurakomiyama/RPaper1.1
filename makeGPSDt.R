library(rjson)
library(data.table)
library(lubridate)
library(ggplot2)
library(sf)
library(dplyr)
my_theme <- function() theme_bw(base_size=10) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())







#==============================#
####       load data        ####
#==============================#

#== load original data ==#
load("C:/Data/spdf.Rdata")

# EROS #
EROS.dt <- fromJSON(file="C:/Data/S2019847_PEROS_3317/EXPORT/EchogramPlot_T20190423_16561743-20190512_19512565.json") 
EROS.dt <- data.table(Latitude = as.numeric(EROS.dt$latitude), Longitude = as.numeric(EROS.dt$longitude), YMD_time = EROS.dt$time)
setDT(EROS.dt)[,YMD_time := as.POSIXct(YMD_time, origin = "1970-01-01", tz = "UTC")]
EROS.dt <- EROS.dt[, lapply(.SD, mean), .(YMD_time = round_date(YMD_time, "10 seconds"))][, lapply(.SD, mean), .(YMD_time = round_date(YMD_time, "10 seconds"))]
#setDT(EROS.dt)[,month := strftime(YMD_time, "%m", tz="UTC")][, time := NULL]


# SD1032 #
sd1032.dt <- as.data.table(read.csv("C:/Data/CRUISE_LOG/sd-1032_GPS_20190424_20190820.csv" , header = TRUE, sep=",", dec="."))
setDT(sd1032.dt)[,YMD_time := as.POSIXct(paste(GPS_date, GPS_time, sep=" "), "%Y-%m-%d %H:%M:%S", tz= "UTC")][, GPS_date := NULL][, GPS_time := NULL]
#setDT(sd1032.dt)[,month := strftime(YMD_time, "%m", tz="UTC")]
sd1032.dt <- sd1032.dt[!Latitude == "na"]


# SD1031 #
gps <- as.data.table(read.csv("C:/Data/CRUISE_LOG/sd-1031_GPS_20190424_20190505.csv" , header = TRUE, sep=",", dec="."))
gps1 <- as.data.table(read.csv("C:/Data/CRUISE_LOG/sd-1031_GPS_20190510_20190626.csv" , header = TRUE, sep=",", dec="."))
gps2 <- as.data.table(read.csv("C:/Data/CRUISE_LOG/sd-1031_GPS_20190628_20190820.csv" , header = TRUE, sep=",", dec="."))
sd1031.dt <- rbind (gps, gps1, gps2)
rm(gps,gps1,gps2)

setDT(sd1031.dt)[,YMD_time := as.POSIXct(paste(GPS_date, GPS_time, sep=" "), "%Y-%m-%d %H:%M:%S", tz= "UTC")][, GPS_date := NULL][, GPS_time := NULL]
#setDT(sd1031.dt)[,month := strftime(YMD_time, "%m", tz="UTC")]
sd1031.dt <- sd1031.dt[!Latitude == "na"]





#=======================#
#### add area column ####
#=======================#
colArea <- function (areadata, data, id_column) {
  dat <- data
  spdf <- areadata
  test <- spdf %>% split(spdf$id) %>% 
    lapply(function(x) rbind(x,x[1,])) %>%
    lapply(function(x) x[,1:2]) %>%
    lapply(function(x) list(as.matrix(x))) %>%
    lapply(function(x) st_polygon(x))
  
  points <- st_as_sf(dat, coords=c('Longitude','Latitude'),remove = F)
  polys <- test %>% st_sfc() %>% st_sf(geom=.) %>% mutate(id=factor(1:13))
  temp <- polys  %>% st_intersection(points) 
  temp <- mutate (temp, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "VestbankenSouthEast",
                                          id=="3" ~ "VestbankenSouthWest", id=="4" ~ "Vestbanken_North",
                                          id=="5" ~ "Vikingbanken", id=="6" ~ "Engelsk_Klondyke",
                                          id=="7" ~ "Inner_Shoal_East_2016", id=="8" ~ "Inner_Shoal_North",
                                          id=="9" ~ "Inner_Shoal_West_2018", id=="10" ~ "Inner_Shoal_test",
                                          id=="11" ~ "Nordgyden", id=="12" ~ "Ostbanken",
                                          TRUE ~ "Outer_Shoal"))
  
  
  temp <- data.table(id_column = temp[[id_column]], area = temp$area)

  temp <- temp[!duplicated(temp[,'id_column']),] # the first area will be remain for data which are on a border of 2 areas
  
  temp2 <- data.table(id_column = dat[[id_column]])
  
  t <- merge(x = temp2, y = temp, by = "id_column", all.x = TRUE)
  
  t$area[is.na(t$area)] = "outside"
  
  setDT(dat)[ ,area := t$area]
  
  setDT(dat)[order(area, YMD_time),]
  
  return(dat)
  
}


EROS.dt <- colArea(areadata = spdf, data = EROS.dt, id_column = "YMD_time")
sd1031.dt <- colArea(areadata = spdf, data = sd1031.dt, id_column = "YMD_time")
sd1032.dt <- colArea(areadata = spdf, data = sd1032.dt, id_column = "YMD_time")


EROS.dt$vessel <- "EROS"
sd1031.dt$vessel <- "SD1031"
sd1032.dt$vessel <- "SD1032"


gps.dt <- rbind (EROS.dt, sd1031.dt, sd1032.dt)
rm(EROS.dt, sd1031.dt, sd1032.dt)



#===========================#
####  add coverage name  ####
#===========================#
colCoverage <- function (data){

  tmp <- as.data.table(data)
  
  v <- unique(tmp$vessel)
  
  setDT(tmp)[, coverage := as.numeric(1)]

  setDT(tmp)[, time_diff := as.numeric(difftime(YMD_time, lag(YMD_time), units = "hours"), by = vessel)]
  
  
  if(sum(is.na(tmp$time_diff), nrow(tmp[tmp$time_diff<0])) > 3 ) {
    print("error in calculating the time difference")
    
  } else {
    
    #== set 0 to the first row of "time_diff" of each vessel ==#
    setDT(tmp)[tmp[, .I[1], by = vessel]$V1, time_diff := 0]
    
    
    #== assign numbers to column "coverage"  ==#
    if (length(v) == 1) {
      v1 <- filter(tmp, vessel == v[1]) #EROS
    
    
    for (i in 2:nrow(v1)){
        #if time_diff is over 24 hours coverage number + 1
        if (v1$time_diff[i] >= 24)
          v1$coverage[i] <- v1$coverage[(i-1)] + 1
        #if time_diff is less than 24h and area[i] is not equal to area[i-1],
        else if (v1$area[i] != v1$area[(i-1)] &&
                 # and if area[i] continues more than 40 in the next 50 rows
                 nrow(filter(v1[i:(i+49)], area == v1$area[i])) >= 40 &&
                 # and if area[i] is not equal to...
                 v1$area[i] != v1[coverage == coverage[i-1]]$area[1])
          #coverage number + 1
          v1$coverage[i] <- v1$coverage[(i-1)] + 1
        
        #if not, coverage number[i] = coverage number [i-1]
        else
          v1$coverage[i] <- v1$coverage[(i-1)]                            
    }
    
    
    } else {
      v1 <- filter(tmp, vessel == v[1]) #EROS
      v2 <- filter(tmp, vessel == v[2]) #SD1031
    v3 <- filter(tmp, vessel == v[3]) #SD1032
    
    for (i in 2:nrow(v1)){
      if (v1$time_diff[i] >= 24)
        v1$coverage[i] <- v1$coverage[(i-1)] + 1
      else if (v1$area[i] != v1$area[(i-1)] &&
               nrow(filter(v1[i:(i+49)], area == v1$area[i]))>=40 &&
               v1$area[i] != v1[coverage == coverage[i-1]]$area[1])
        v1$coverage[i] <- v1$coverage[(i-1)] + 1
      else
        v1$coverage[i] <- v1$coverage[(i-1)]                            
    }
    

    for (i in 2:nrow(v2)){
      if (v2$time_diff[i] >= 24)
        v2$coverage[i] <- v2$coverage[(i-1)] + 1
      else if (v2$area[i] != v2$area[(i-1)] &&
               nrow(filter(v2[i:(i+49)], area == v2$area[i]))>=40 &&
               v2$area[i] != v2[coverage == coverage[i-1]]$area[1])
        v2$coverage[i] <- v2$coverage[(i-1)] + 1
      else
        v2$coverage[i] <- v2$coverage[(i-1)]                            
    }
    
    
    for (i in 2:nrow(v3)){
      if (v3$time_diff[i] >= 24)
        v3$coverage[i] <- v3$coverage[(i-1)] + 1
      else if (v3$area[i] != v3$area[(i-1)] &&
               nrow(filter(v3[i:(i+49)], area == v3$area[i]))>=40 &&
               v3$area[i] != v3[coverage == coverage[i-1]]$area[1])
        v3$coverage[i] <- v3$coverage[(i-1)] + 1
      else
        v3$coverage[i] <- v3$coverage[(i-1)]                            
    }
    
    }
    
    
    tmp <- if(length(v) == 1) data.table(v1) else rbind(v1, v2, v3)
    
    
    #== coverage continuously traveled -> split into 2 by time ==#
    
    m <- max(tmp$coverage, na.rm = TRUE)
    setDT(tmp)[, area_most := names(which.max(table(area))), by=c("coverage", "vessel")]
    
    setDT(tmp)[area_most == "Engelsk_Klondyke" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-05-01 09:20:00", tz="UTC") & YMD_time <= as.POSIXct("2019-05-03 20:25:00", tz="UTC"), coverage := m + 1]
    setDT(tmp)[area_most == "Engelsk_Klondyke" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-06-11 19:32:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-15 05:44:00", tz="UTC"), coverage := m + 2]
    setDT(tmp)[area_most == "Vikingbanken" & vessel == "SD1031" & YMD_time >= as.POSIXct("2019-06-02 20:57:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-04 08:43:00", tz="UTC"), coverage := m + 3]
    setDT(tmp)[area_most == "Vikingbanken" & vessel == "SD1031" & YMD_time >= as.POSIXct("2019-06-04 08:44:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-07 08:20:00", tz="UTC"), coverage := m + 4]
    


    
    
    #== same coverage split into multiple -> merge into one ==#
    
    setDT(tmp)[area_most == "Vikingbanken" & vessel == "EROS" & (YMD_time < as.POSIXct("2019-05-11 19:48:20", tz="UTC") | YMD_time > as.POSIXct("2019-05-12 03:45:00", tz="UTC")), coverage := m + 11]
    
    setDT(tmp)[area_most == "AlbjoernLing" & vessel == "EROS" & (YMD_time < as.POSIXct("2019-05-08 20:16:50", tz="UTC") | YMD_time > as.POSIXct("2019-05-09 02:06:10", tz="UTC")), coverage := m + 12]
    
    setDT(tmp)[area_most == "AlbjoernLing" & vessel == "SD1032", coverage := m + 13]
    
    setDT(tmp)[area_most == "Ostbanken" & vessel == "EROS" & YMD_time > as.POSIXct("2019-05-08 04:18:00", tz="UTC"), coverage := m + 14]
    
    setDT(tmp)[area_most == "Nordgyden" & vessel == "EROS", coverage := m + 15]
    
    setDT(tmp)[area_most == "Outer_Shoal" & vessel == "EROS" & YMD_time <= as.POSIXct("2019-04-25 18:37:00", tz="UTC"), coverage := m + 16]

    setDT(tmp)[area_most == "Outer_Shoal" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-05-17 06:56:00", tz="UTC") & YMD_time <= as.POSIXct("2019-05-21 02:59:00", tz="UTC"), coverage := m + 17]
    setDT(tmp)[area_most == "Outer_Shoal" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-06-21 06:40:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-26 08:24:00", tz="UTC"), coverage := m + 18]
    
    setDT(tmp)[area_most == "Inner_Shoal_East_2016" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-06-02 06:32:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-06 14:39:00", tz="UTC"), coverage := m + 19]
    
    setDT(tmp)[area_most == "Inner_Shoal_East_2016" & vessel == "EROS" & YMD_time <= as.POSIXct("2019-04-29 18:51:30", tz="UTC"), coverage := m + 20]
    
    setDT(tmp)[area_most == "Inner_Shoal_East_2016" & vessel == "SD1031" & YMD_time >= as.POSIXct("2019-05-21 04:45:00", tz="UTC") & YMD_time <= as.POSIXct("2019-05-23 16:10:00", tz="UTC"), coverage := m + 21]
    
    
    setDT(tmp)[area_most == "Inner_Shoal_West_2018" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-06-07 05:42:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-10 20:17:00", tz="UTC"), coverage := m + 22]
    
    setDT(tmp)[area_most == "Inner_Shoal_West_2018" & vessel == "SD1031" & YMD_time >= as.POSIXct("2019-05-24 04:36:00", tz="UTC") & YMD_time <= as.POSIXct("2019-05-26 14:26:00", tz="UTC"), coverage := m + 23]
    

    setDT(tmp)[area_most == "Vestbanken_North" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-05-06 18:26:10", tz="UTC") & YMD_time <= as.POSIXct("2019-05-06 23:12:20", tz="UTC"), coverage := m + 24]
    
    setDT(tmp)[area_most == "Vestbanken_North" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-04-26 04:42:40", tz="UTC") & YMD_time <= as.POSIXct("2019-04-26 17:27:10", tz="UTC"), coverage := m + 25]
    
    setDT(tmp)[area_most == "Vestbanken_North" & vessel == "SD1032", coverage := m + 26]
    
    setDT(tmp)[area_most == "Vestbanken_North" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-05-24 21:02:00", tz="UTC"), coverage := m + 27]
    
    setDT(tmp)[!area == "outside" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-04-28 02:32:00", tz="UTC") & YMD_time <= as.POSIXct("2019-04-28 17:15:00", tz="UTC"), coverage := m + 28]
    
    setDT(tmp)[area_most == "VestbankenSouthWest" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-04-27 04:18:30", tz="UTC") & YMD_time <= as.POSIXct("2019-04-27 19:10:00", tz="UTC"), coverage := m + 29]
    setDT(tmp)[area_most == "VestbankenSouthWest" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-05-02 03:38:20", tz="UTC") & YMD_time <= as.POSIXct("2019-05-02 18:58:30", tz="UTC"), coverage := m + 30]
    
    setDT(tmp)[area_most == "Engelsk_Klondyke" & vessel == "EROS" & YMD_time >= as.POSIXct("2019-05-07 04:40:10", tz="UTC") & YMD_time <= as.POSIXct("2019-05-07 16:53:00", tz="UTC"), coverage := m + 31]
    setDT(tmp)[area_most == "Engelsk_Klondyke" & vessel == "SD1032" & YMD_time >= as.POSIXct("2019-05-12 20:33:00", tz="UTC") & YMD_time <= as.POSIXct("2019-05-17 01:45:00", tz="UTC"), coverage := m + 32]

   #=====================================# 

    
    
    #== outside -> "F" ==#
    tmp[, coverage := as.character(coverage)][area_most == "outside", coverage := as.character("F")]    
   
    setDT(tmp)[, StartTime:=min(YMD_time), by=c("vessel", "coverage")]
   
    #outside <- tmp[area%in%"outside"]
    #outside$coverage <- as.character(outside$coverage)
    #outside$coverage <- "F"
    #tmp <- data.table(rbind(tmp[!area%in%"outside"], outside))
    #tmp <- tmp[order(vessel, YMD_time),]
    
    
    
    

    #== coverage names ==#

    spdf.tmp <- data.table(Latitude = spdf$lat, Longitude = spdf$long, area = spdf$area)
    setDT(spdf.tmp)[, c("north", "south", "east", "west") := 
                      list(max(Latitude) - ((max(Latitude)-min(Latitude)) * 0.2),
                           min(Latitude) + ((max(Latitude)-min(Latitude)) * 0.2),
                           max(Longitude) - ((max(Longitude)-min(Longitude)) * 0.2),
                           min(Longitude) + ((max(Longitude)-min(Longitude)) * 0.2)),
                    by=c("area")]
    setDT(spdf.tmp)[, Latitude := NULL][, Longitude := NULL]
    spdf.tmp <- unique(spdf.tmp)
    colnames(spdf.tmp)[which(colnames(spdf.tmp) %in% c("area") )] <- c("area_most")
    
    
    
    tmp <- tmp |> left_join(spdf.tmp, by = "area_most")
    
    
    
    setDT(tmp)[, northernmost := max(Latitude), by=c("coverage", "vessel")][, southernmost := min(Latitude), by=c("coverage", "vessel")][, easternmost := max(Longitude), by=c("coverage", "vessel")][, westernmost := min(Longitude), by=c("coverage", "vessel")]
    
    
    
    #== test if a coverage covers most of the area ==#
    
    setDT(tmp)[, coverage_test := as.character(coverage)][, coverage_test := fifelse( north <= northernmost & south >= southernmost & east <= easternmost & west >= westernmost, coverage_test, paste(coverage_test,"x", sep = ".")),  by=c("coverage", "vessel")]
    
    setDT(tmp)[vessel == "EROS" & YMD_time >= as.POSIXct("2019-05-06 18:26:10", tz="UTC") & YMD_time <= as.POSIXct("2019-05-06 23:12:20", tz="UTC"), coverage_test := paste(coverage_test,"x", sep = ".")]
    
    setDT(tmp)[vessel == "SD1032" & YMD_time >= as.POSIXct("2019-06-26 15:51:00", tz="UTC") & YMD_time <= as.POSIXct("2019-06-27 08:35:00", tz="UTC"), coverage_test := paste(coverage_test,"x", sep = ".")]
    
    setDT(tmp)[vessel == "EROS" & YMD_time >= as.POSIXct("2019-04-25 18:37:10", tz="UTC") & YMD_time <= as.POSIXct("2019-04-26 02:31:20", tz="UTC"), coverage_test := paste(coverage_test,"x", sep = ".")]
    
    setDT(tmp)[vessel == "EROS" & YMD_time > as.POSIXct("2019-04-28 17:15:00", tz="UTC") & YMD_time <= as.POSIXct("2019-04-28 23:13:00", tz="UTC"), coverage_test := paste(coverage_test,"x", sep = ".")]


    
    
    
    #lengths(regmatches(names(table(tmp$coverage_test)), gregexpr("x", names(table(tmp$coverage_test)))))
    
    
    #setDT(tmp)[, coverage_test := as.character(coverage)][, coverage_test := ifelse(sum(.N)<=1000, paste(coverage_test,"x", sep = "."), coverage_test), by=c("coverage", "vessel")]
    
    
    setDT(tmp)[, coverage_no := paste(vessel, coverage_test, sep = "-")]
    

    name <- as.data.table(as.table(with(tmp, by(area, coverage_no, function(xx)names(which.max(table(xx)))))))
    temp <- as.data.table(as.table(with(tmp, by(StartTime, coverage_no, function(xx)names(which.max(table(xx)))))))
    setDT(name)[, StartTime := as.POSIXct(temp$N, tz= "UTC")]

    setDT(name)[, vessel := gsub('-.*',"",coverage_no)]
    setDT(name)[, complete_coverage := ifelse(endsWith(name$coverage_no, "x"), "x", "c")]
    setDT(name)[, group_no := order(StartTime), by  = c("complete_coverage","N","vessel")]
    setDT(name)[, coverage := ifelse(gsub('.*-',"",name$coverage_no) == ("F") , 
                                     coverage_no, paste(vessel, N, group_no, sep="-"))]
    setDT(name)[, coverage := ifelse(endsWith(name$coverage_no, "x"),
                                     paste(coverage, "x", sep = "."), coverage)]
    name <- data.table(coverage_no = name$coverage_no, coverage_name = name$coverage)
    
    
    #== merge with tmp ==#
    tmp <- merge(x = tmp, y = name, by = "coverage_no", all.x = TRUE)
    
    
    
    
    
    
    
    
    #== add start and end time ==#
    setDT(tmp)[, StartTime:=min(YMD_time), by=(coverage_name)]
    setDT(tmp)[, StopTime:=max(YMD_time), by=(coverage_name)]
    
    
    #== add distance column ==#
    library(geosphere)
    tmp <- tmp[order(vessel, coverage_name, YMD_time),]
    tmp$distance[2:nrow(tmp)] <- sapply(2:nrow(tmp), function(x) distm(tmp[x-1, c('Longitude', 'Latitude')], tmp[x, c('Longitude', 'Latitude')], fun = distHaversine))
    
    setDT(tmp)[tmp[, .I[1], by=c("coverage_name","vessel")]$V1, distance:=0]
    setDT(tmp)[, distance_sum:=sum(distance), by = c("coverage_name", "vessel")]
    
    #== remove unnecessary column ==#
    setDT(tmp)[, c("coverage_test", "coverage_no", "coverage", "time_diff", "north", "south", "east", "west", "northernmost", "southernmost", "easternmost", "westernmost") := NULL]
    
    tmp <- tmp[order(vessel, YMD_time),]
    
    return(tmp)
    
    
  }
  
}



gps.dt <- colCoverage(gps.dt)

gps.dt[, .(cnt= sum(.N)), by= c("vessel", "coverage_name")]





#================#
####   plot   ####
#================#
label <- data.table(coverage_name=unique(gps.dt$coverage_name), StartTime = unique(gps.dt$StartTime), StopTime=unique(gps.dt$StopTime), distance_sum = unique(gps.dt$distance_sum))
label[gps.dt, on = 'coverage_name', area_most := area_most][gps.dt, on = 'coverage_name', vessel := vessel]
label <- label[order(area_most, coverage_name),]
setDT(label)[, complete_coverage := ifelse(endsWith(coverage_name, "x"), "x", "c")]



## "Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"

a <- c("Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal", "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018", "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest") #
v <- c("SD1032", "EROS","SD1031") # 


spdf_tmp <- data.table()
for (i in 1:length(a)) {
  tmp <- data.table(long = rep(subset(spdf$long, spdf$area %in% a[i]), length(unique(label[area_most %in% a[i] & vessel %in% v]$coverage_name))), 
                    lat = rep(subset(spdf$lat, spdf$area %in% a[i]), length(unique(label[area_most %in% a[i] & vessel %in% v]$coverage_name))), 
                    coverage_name = rep(label[area_most %in% a[i] & vessel %in% v]$coverage_name, each = length(subset(spdf$long, spdf$area %in% a[i]))) 
  )
  tmp$area <- gsub(".*-(.*)\\-.*", "\\1", tmp$coverage_name)
  tmp$vessel <- gsub("\\-.*","" , tmp$coverage_name)
  spdf_tmp <- rbind(spdf_tmp, tmp)
  rm(tmp)
}


ggplot() + my_theme() + theme(axis.title = element_blank()) +
  geom_polygon(data = spdf_tmp[area %in% a & !coverage_name %like% "x"], aes(long,lat, group=coverage_name), col="black", alpha=.2) +
  geom_point(data = gps.dt[area_most %in% a & vessel %in% v & !coverage_name %like% "x"], 
             aes(x = Longitude, y = Latitude), size=.05, alpha =0.2, colour = "red") +
  geom_text(data = label[area_most %in% a & vessel %in% v & !coverage_name %like% "x"], aes(x = -Inf, y = Inf, label = StartTime), hjust   = 0, vjust   = 1) + 
  geom_text(data = label[area_most %in% a & vessel %in% v & !coverage_name %like% "x"], aes(x = -Inf, y = -Inf, label = StopTime), hjust   = 0, vjust   = 0) + 
  #geom_text(data = label[area_most %in% a & vessel %in% v & !coverage_name %like% "x"], aes(x = -Inf, y = Inf, label = round(distance_sum, digits = 0)), hjust   = 0.7, vjust   = 0) + 
  facet_wrap(~coverage_name, scales="free")







save(gps.dt, file="gps.Rdata")
rm(name, temp, gps_1.dt, text, label)















#================================================================#
####                    test code                             ####
#================================================================#
#label <- data.table(aggregate(gps.dt$YMD_time ~ gps.dt$coverage_name + gps.dt$vessel, FUN = min), aggregate(gps.dt$YMD_time ~ gps.dt$coverage_name + gps.dt$vessel, FUN = max)[3])
#colnames(label) <- c("coverage_name", "vessel", "start", "end")
#label[gps.dt, on = 'coverage_name', area_most := area_most][gps.dt, on = 'coverage_name', distance_sum := distance_sum]
