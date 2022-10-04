rm(list=ls()) # remove all the variables from R environment
#== random sampling ==#
#sample(unique(School_rf.dt$id),1)   ## choose 1 from all "id"
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

# test text

#===============================#
####        Load data        ####
#===============================#

library(dplyr)
library(data.table)
library(ggplot2)

#setwd("C:/Users/a37907/Desktop/KnowSandeel15781/Data")
#setwd("E:/KnowSandeel15781/Data")
#setwd("C:/Users/komiy/Desktop/KnowSandeel15781/Data")

#==========================================================================================================================================#
#### vessels gps     ####
#==========================================================================================================================================#

#SvSchool.dt <- as.data.table(read.csv("Svschool_50.csv",header = TRUE, sep = ","))


#== route of vessels ==#
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


#== when load "EROS.df" / "sd1031.df" / "sd1032.df" ==#
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


#== run by vessel ==#
v = as.character("EROS") # "SD1031" / "SD1032" / "EROS"
tmp <- gps.dt[vessel%in%v] 

#==============================================================#
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
#==============================================================#

tmp[, .(cnt= sum(.N)), by= c("coverage", "area")]

outside <- gps.dt[vessel%in%v & area%in%"outside"]
outside$coverage<- as.character(outside$coverage)
outside$coverage<- "F"

tmp <- data.table(rbind(tmp, outside)) #change data.table name depending on vessel "E":EROS, "s1":SD1031, "s2":SD1032
assign(paste("tmp", v, sep = "_"), tmp)


## combine 3 data.table
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
# merge with gps.dt
gps.dt <- merge(x = gps.dt, y = temp, by = "coverage_no", all.x = TRUE)
# coverage continuously traveled -> split into 2 by time
setDT(gps.dt)[, coverage_name := ifelse(coverage_name == "SD1032-Engelsk_Klondyke-3", ifelse(YMD_time<=as.POSIXct('2019-06-14 23:59:59', tz="UTC"), "SD1032-Engelsk_Klondyke-3", "SD1032-Engelsk_Klondyke-4") ,coverage_name)]
setDT(gps.dt)[, coverage_name := ifelse(coverage_name == "SD1031-Vikingbanken-1", ifelse(YMD_time<=as.POSIXct('2019-06-04 08:43:00', tz="UTC"), "SD1031-Vikingbanken-1", "SD1031-Vikingbanken-2") ,coverage_name)]
setDT(gps.dt)[, coverage_name := ifelse(coverage_name %in% c("SD1031-Inner_Shoal_North-1", "SD1031-VestbankenSouthWest-1", "SD1031-VestbankenSouthWest-2"), "Transect", coverage_name)]
# add start and end time
setDT(gps.dt)[, StartTime:=min(YMD_time), by=(coverage_name)]
setDT(gps.dt)[, StopTime:=max(YMD_time), by=(coverage_name)]
# add distance column #
library(geosphere)
gps.dt <- gps.dt[order(vessel,coverage_name, YMD_time),]
gps.dt$distance[2:nrow(gps.dt)] <- sapply(2:nrow(gps.dt), function(x) distm(gps.dt[x-1, c('Longitude', 'Latitude')], gps.dt[x, c('Longitude', 'Latitude')], fun = distHaversine))
setDT(gps.dt)[gps.dt[, .I[1], by=c("coverage_name","vessel")]$V1, distance:=0]
setDT(gps.dt)[, distance_sum:=sum(distance), by = c("coverage_name", "vessel")]

#plot with start end time
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

#==========================================================================================================================================#
#### environment data   ####
#==========================================================================================================================================#

library(ncdf4)
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
ctd_EROS.dt$start_time <- ifelse(str_sub(ctd_EROS.dt$start_time,-1,-1)=="-",substr(ctd_EROS.dt$start_time,1,nchar(ctd_EROS.dt$start_time)-1),ctd_EROS.dt$start_time)
ctd_EROS.dt$start_time <- as.POSIXct(ctd_EROS.dt$start_time, "%Y-%m-%d %H:%M:%S", tz= "UTC")

ctd_EROS.dt$Latitude <- as.numeric(substr(ctd_EROS.dt$Latitude, 1,2)) + (as.numeric(substr(ctd_EROS.dt$Latitude, 4,5))/60) + (as.numeric(substr(ctd_EROS.dt$Latitude, 7,8))/3600)
ctd_EROS.dt$Longitude <- as.numeric(substr(ctd_EROS.dt$Longitude, 1,3)) + (as.numeric(substr(ctd_EROS.dt$Longitude, 5,6))/60) + (as.numeric(substr(ctd_EROS.dt$Longitude, 8,9))/3600)
save(ctd_EROS.dt, file="ctd_EROS.Rdata")
#
#




#==========================================================================================================================================#
#### Eros data  ####
#==========================================================================================================================================#

#== create empty data table
SvSchool.dt <- data.table()
#== This pattern will be used in loop when extracting school ID from file name
#== pattern : extract strings between "SvSchool" and "_T2019"
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"

#== Read file name and directory of text data
files <- list.files('C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019847_PEROS_3317/EXPORT/Sv_SAND_2.11.0-rc1',pattern='txt$', full.names=TRUE)
#files <- list.files('S2019847_PEROS_3317/EXPORT/test', pattern='txt$', full.names=TRUE)

#== read SAND data file(i) ==#
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "SAND", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool.dt <- rbind(SvSchool.dt, temp, fill=TRUE)
  temp <- data.table()
}
SvSchool.dt <- SvSchool.dt[!is.na(SvSchool.dt$value), ]
save(SvSchool.dt, file = "Svschool_SAND.Rdata")
rm(files, i, pattern, temp)

#== read PSAND data file(i) ==#
SvSchool_PSAND.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019847_PEROS_3317/EXPORT/Sv_PSAND_2.11.0-rc1',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "PSAND", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_PSAND.dt <- rbind(SvSchool_PSAND.dt, temp, fill=TRUE)
  temp <- data.table()
}
SvSchool_PSAND.dt <- SvSchool_PSAND.dt[!is.na(SvSchool_PSAND.dt$value), ]
save(SvSchool_PSAND.dt, file = "Svschool_PSAND.Rdata")
rm(files, i, pattern, temp)

#== read OTHER data file(i) ==#
SvSchool_OTHER.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019847_PEROS_3317/EXPORT/Sv_OTHER_2.11.0-rc1',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "OTHER", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_OTHER.dt <- rbind(SvSchool_OTHER.dt, temp, fill=TRUE)
  temp <- data.table()
}
SvSchool_OTHER.dt <- SvSchool_OTHER.dt[!is.na(SvSchool_OTHER.dt$value), ]
save(SvSchool_OTHER.dt, file = "Svschool_OTHER.Rdata")
rm(files, i, pattern, temp)

#ここから
#============================================#
#### Read raw data of 3 categories (EROS) ####
#============================================#
lapply(c("C:/Users/a37907/Desktop/KnowSandeel15781/Data/Svschool_SAND.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/Svschool_PSAND.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/Svschool_OTHER.Rdata"),load,.GlobalEnv)
SvSchool.dt <- rbind(SvSchool.dt, SvSchool_PSAND.dt, SvSchool_OTHER.dt)
setnames(SvSchool.dt, c("variable", "value"), c("SampleNo","Sv"))
rm(SvSchool_PSAND.dt, SvSchool_OTHER.dt)


#  calculate all variables and create school data.table
#         |          |         |
#         v          v         v
#         |          |         |
#         v          v         v


#==========================================================================================================================================#
#### Saildrone data 1032   ####
#==========================================================================================================================================#


#== read KORONA data file(i) ==#
SvSchool.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1032/EXPORT/PSAND',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "KORONA", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool.dt <- rbind(SvSchool.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool.dt, file = "Svschool_KORONA.Rdata")



#== read manual data file(i) ==#
SvSchool_manual.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019_SAILDRONE_1032/EXPORT/SAND',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "manual", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_manual.dt <- rbind(SvSchool_manual.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool_manual.dt, file = "Svschool_manual.Rdata")



#== read bubble/noise data file(i) ==#
SvSchool_noise.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019_SAILDRONE_1032/EXPORT/OTHER(bubble,noise)',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "noise", temp)                                   ## add column "category"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_noise.dt <- rbind(SvSchool_noise.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool_noise.dt, file = "SvSchool_noise.Rdata")


#ここから
#==============================================#
#### Read raw data of 3 categories (SD1032) ####
#==============================================#
lapply(c("C:/Users/a37907/Desktop/KnowSandeel15781/Data/Svschool_KORONA.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/Svschool_manual.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_noise.Rdata"),load,.GlobalEnv)

SvSchool_1032.dt <- rbind(SvSchool.dt, SvSchool_manual.dt, SvSchool_noise.dt)
SvSchool_1032.dt <- SvSchool_1032.dt[!is.na(SvSchool_1032.dt$value), ]
setnames(SvSchool_1032.dt, c("variable", "value"), c("SampleNo","Sv"))
rm(SvSchool.dt, SvSchool_manual.dt, SvSchool_noise.dt)


#length(unique(SvSchool_1032.dt[["PingNumber"]]))
#length(unique(SvSchool_1032.dt[["id"]]))


#  calculate all variables and create school data.table
#         |          |         |
#         v          v         v
#         |          |         |
#         v          v         v



#==========================================================================================================================================#
#### Saildrone data 1031   ####
#==========================================================================================================================================#

#== read KORONA data file(i) ==#
SvSchool_1031_KORONA.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1031/EXPORT/PSAND',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "KORONA", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_1031_KORONA.dt <- rbind(SvSchool_1031_KORONA.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool_1031_KORONA.dt, file = "SvSchool_1031_KORONA.Rdata")



#== read manual data file(i) ==#
SvSchool_1031_manual.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019_SAILDRONE_1031/EXPORT/SAND',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "manual", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_1031_manual.dt <- rbind(SvSchool_1031_manual.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool_1031_manual.dt, file = "SvSchool_1031_manual.Rdata")



#== read bubble/noise data file(i) ==#
SvSchool_1031_noise.dt <- data.table()
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
files <- list.files('S2019_SAILDRONE_1031/EXPORT/OTHER',pattern='txt$', full.names=TRUE)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "noise", temp)                                   ## add column "category"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  SvSchool_1031_noise.dt <- rbind(SvSchool_1031_noise.dt, temp, fill=TRUE)
  temp <- data.table()
}
save(SvSchool_1031_noise.dt, file = "SvSchool_1031_noise.Rdata")


#ここから
#==============================================#
#### Read raw data of 3 categories (SD1031) ####
#==============================================#
lapply(c("C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_1031_KORONA.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_1031_manual.Rdata", 
         "C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_1031_noise.Rdata"),load,.GlobalEnv)

SvSchool_1031.dt <- rbind(SvSchool_1031_KORONA.dt, SvSchool_1031_manual.dt, SvSchool_1031_noise.dt)
SvSchool_1031.dt <- SvSchool_1031.dt[!is.na(SvSchool_1031.dt$value), ]
setnames(SvSchool_1031.dt, c("variable", "value"), c("SampleNo","Sv"))
rm(SvSchool_1031_KORONA.dt, SvSchool_1031_manual.dt, SvSchool_1031_noise.dt)
















#  calculate all variables and create school data.table
#         |          |         |
#         v          v         v
#==========================================================================================================================================#

#== input data.table ==#
#Sv.dt <- SvSchool.dt
#Sv.dt <- SvSchool_1032.dt
Sv.dt <- SvSchool_1031.dt
#== input data.table ==#
library("rjson")
#bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019847_PEROS_3317/EXPORT/bottomEchogramPlot_T20190423_16561743-20190512_19512565.json") #EROS
#bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json") #SD1032  
bottom.json <- fromJSON(file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_1031/EXPORT/bottomEchogramPlot_T20190424_10291908-20190820_12575243.json")  #SD1031


#== calculate depth_bin of pixel ==#
Sv.dt$depth_bin <- (Sv.dt$DepthStop- Sv.dt$DepthStart)/Sv.dt$SampleCount
Sv.dt$depthStart <- Sv.dt$DepthStart + (Sv.dt$depth_bin * (as.numeric(gsub("Sv", "", Sv.dt$SampleNo))-1))
Sv.dt$depthStop <- Sv.dt$depthStart + Sv.dt$depth_bin
#== calculate sV from SV ==#
setDT(Sv.dt)[, sV := 10^(Sv/10)]
#== frequency response by each ping & each sample  ==#
setDT(Sv.dt)[, sV38 := sV[Frequency==38], by = c("id", "PingNumber", "SampleNo")]
#== frequency response by school id use mean sV of school  ==#
setDT(Sv.dt)[, sV_mean := mean(sV), by = c("id", "Frequency")][, rf := sV_mean/sV_mean[Frequency==38], by = c("id")]
setDT(Sv.dt)[, sV_max := max(sV), by = c("id", "Frequency")][, sV_min := min(sV), by = c("id", "Frequency")]
setDT(Sv.dt)[, PingNo := max(PingNumber)-min(PingNumber) + 1, by ="id"][, sV_stdv := sd(sV), by = c("id", "Frequency")]
setDT(Sv.dt)[, SE_f := sV_stdv/sqrt(PingNo*SampleCount), by = c("id", "Frequency")][, SE := SE_f/mean(sV38), by = c("id", "Frequency")] #standard error of
setDT(Sv.dt)[, sV_var := var(sV), by = c("id", "Frequency")]
setDT(Sv.dt)[, pixelNo := length(sV), by = c("id", "Frequency")]
#== time ==#
library(stringr)
setDT(Sv.dt)[,da:=paste(str_sub(Date, 1,4), str_sub(Date, 5,6), str_sub(Date, 7,8), sep="-")]
setDT(Sv.dt)[,Time:=as.integer(Time)][,Time := (ifelse(nchar(Time)==7, paste0("0", Time), Time))]
setDT(Sv.dt)[,Time := (ifelse(nchar(Time)==6, paste0("00", Time), Time))][,Time := (ifelse(nchar(Time)==5, paste0("000", Time), Time))][,Time := (ifelse(nchar(Time)==4, paste0("0000", Time), Time))][,Time := (ifelse(nchar(Time)==3, paste0("00000", Time), Time))]
setDT(Sv.dt)[,t:=paste(str_sub(Time, -8,-7), str_sub(Time, -6,-5), str_sub(Time, -4,-3), sep=":")][,t:=paste(t, str_sub(Time, -2,-1), sep=".")]
setDT(Sv.dt)[,YMD_time:=paste(da, t, sep=" ")][,YMD_time:=as.POSIXct(YMD_time, "%Y-%m-%d %H:%M:%S", tz= "UTC")][,da:=NULL][,t:=NULL]
#== Distance between pings ==#
library("rjson")
bottom.dt <- data.table(PingNumber = bottom.json$pingNumber,  PingDistance = as.numeric(bottom.json$vesselDistance))
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
bottom.dt <- data.table(PingNumber = bottom.json$pingNumber, BottomDepth = as.numeric(bottom.json$lowerLayerBoundary))
Sv.dt <- merge(x = Sv.dt, y = bottom.dt, by = "PingNumber", all.x = TRUE)
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
#
#== !! save all pixel data !!choose correct name!! ==#
#save(Sv.dt, file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_EROS.Rdata") 
#save(Sv.dt, file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_1032.Rdata")
save(Sv.dt, file="C:/Users/a37907/Desktop/KnowSandeel15781/Data/SvSchool_1031.Rdata")


#== make r(f) data table ==#
School.dt <- with(Sv.dt, aggregate(Sv.dt[,c("Latitude", "Longitude", "YMD_time", "weighted_meanDepth","nor_Depth","nor_DepthStart", "nor_DepthStop", "DepthfromBottom")], 
                                   list(id, category, Frequency, Date, PingNo, pixelNo, SampleCount,DepthStart, DepthStop, sV_mean,sV_max, sV_min, sV_stdv, sV_var, rf,SE,sA, school_area, school_length, meanDepth, Perimeter, Fractal), mean))
School.dt <- as.data.table(School.dt)
setnames(School.dt, c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5", 
                      "Group.6", "Group.7","Group.8", "Group.9","Group.10", 
                      "Group.11", "Group.12", "Group.13", "Group.14", "Group.15", 
                      "Group.16",  "Group.17", "Group.18", "Group.19", "Group.20", 
                      "Group.21", "Group.22"), 
         c("id", "category", "Frequency", "Date", "PingNo", 
           "pixelNo", "SampleCount","DepthStart", "DepthStop","sV_mean",
           "sV_max", "sV_min", "sV_stdv", "sV_var", "rf", 
           "SE","sA","school_area", "school_length", "meanDepth", 
           "Perimeter","Fractal"))
School.dt <- School.dt[order(id),]
attr(School.dt$YMD_time, "tzone") <- "UTC"

#== sun altitude ==#
latlon <- subset(School.dt, select=c(Latitude, Longitude, YMD_time))
colnames(latlon) <- c("lat", "lon", "date")
library("suncalc")
altitude.df <- getSunlightPosition(data=latlon, keep = c("altitude"))
School.dt <- cbind(School.dt, altitude.df)
School.dt <- subset(School.dt, select=-c(lat,lon, date))
rm(latlon, altitude.df)
School.dt$altitude_degree <- School.dt$altitude*180/pi #convert from radian to degree

##== add area ==##
#= read area info and classify data to the areas =#
library(geojsonio)
library(broom)
spdf <- tidy(geojson_read("C:/Users/a37907/Desktop/KnowSandeel15781/Data/StratumPolygon.geojson",  what = "sp"))
spdf <- mutate (spdf, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                        id=="3" ~ "Inner_Shoal_East_2016", id=="4" ~ "Inner_Shoal_North",
                                        id=="5" ~ "Inner_Shoal_West_2018", id=="6" ~ "Inner_Shoal_test",
                                        id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                        id=="9" ~ "Outer_Shoal", id=="10" ~ "VestbankenSouthEast",
                                        id=="11" ~ "VestbankenSouthWest", id=="12" ~ "Vestbanken_North",
                                        TRUE ~ "Vikingbanken"))

library("sf")
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
rm(test, points, polys, temp, temp2, t)
#== school height, Elongation, Rectangularity =##
setDT(School.dt)[, school_height:=DepthStop-DepthStart][, Elongation:=school_length/school_height]
setDT(School.dt)[, school_rect:=(school_length*school_height)/school_area]
setDT(School.dt)[, school_circ:=(Perimeter^2)/(4*pi*school_area)]

#== !! add vessel column !!choose correct vessel name!! ==#
School.dt$vessel <- "SD1031" #"EROS"/"SD1032"/"SD1032"

#== add bottom depth ==#
School.dt$bottom_Depth <- School.dt$weighted_meanDepth / School.dt$nor_Depth


#== save all pixel data !!choose correct name!! ==#
#save(School.dt, file = "Data/School_EROS.Rdata")
#save(School.dt, file = "C:/Users/a37907/Desktop/KnowSandeel15781/Data/School_1032.Rdata")
save(School.dt, file = "C:/Users/a37907/Desktop/KnowSandeel15781/Data/School_1031.Rdata")

#sum(Sv.dt$Frequency == 200 & Sv.dt$rf>1)   ## number of samples: r(f)>1.0 at 200kHz

rm(Sv.dt, School.dt, bottom.json, bottom.dt, SvSchool.dt, SvSchool_1032.dt, SvSchool_1031.dt)


#== combine SD1031 and SD1032 + assign new id number ==#
load("C:/Users/a37907/Desktop/KnowSandeel15781/Data/School_1032.Rdata")
School_1032.dt <- School.dt

load("C:/Users/a37907/Desktop/KnowSandeel15781/Data/School_1031.Rdata")
School_1031.dt <- School.dt

School_SD.dt <- rbind(School_1031.dt, School_1032.dt)
School_SD.dt$original_id <- School_SD.dt$id
setDT(School_SD.dt)[, id:= 1:.N, by=Frequency]

save(School_SD.dt, file="Data/School_SD.Rdata")

rm(School.dt, School_1031.dt, School_1032.dt)

#==========================================================================================================================================#
#==========================================================================================================================================#
#==========================================================================================================================================#




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
####   biological data    ####
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
#setDT(sta.ind_1)[,mo:=str_sub(sta.ind_1$stationstartdate,6,7)][, da:=str_sub(sta.ind_1$stationstartdate,9,10)][,Time_start:=paste(startyear, mo, da, sep="-")]
#setDT(sta.ind_1)[,Time_start:=paste(Time_start, stationstarttime, sep=" ")]
#setDT(sta.ind_1)[,Time_start:=as.POSIXct(sta.ind_1$Time_start, "%Y-%m-%d %H:%M", tz= "UTC")]
#setDT(sta.ind_1)[,Time_end:=paste(startyear, mo, da, sep="-")][,Time_end:=paste(Time_end, stationstoptime, sep=" ")]
#setDT(sta.ind_1)[,Time_end:=as.POSIXct(sta.ind_1$Time_end, "%Y-%m-%d %H:%M", tz= "UTC")][,mo:=NULL][,da:=NULL]










#================================#
####   find Sandeel school    ####
#================================#

data <- subset(School_EROS.dt, Frequency %in% 200)
summary(10*log10(data$sV_mean))
#EROS : -83.98(Min). -17.60(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-84, -17, 1), labels=as.character(seq(-83, -17, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
table(data$category)


data_1032 <- subset(School_1032.dt, Frequency %in% 200)
summary(10*log10(data_1032$sV_mean))
#1032 : -75.02(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-76, -20, 1), labels=as.character(seq(-75, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
table(data_1032$category)


#== Is the school size reasonable? ==#
# EROS #
nrow(data[data$category=="SAND"])/length(unique(School_EROS.dt[category=="SAND"]$id)) # detection rate
# saildrone #
sum(data_1032$sA)/sum(School_1032.dt[Frequency==200]$sA)


#== frequency response(rf) ==#
# EROS #
#test <- data[rf>= 1]
test <- data[rf>= 0.8 & rf<= 3.5] #rf>= 0.85 & rf<= 1.6
nrow(test[category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[rf>= 0.8 & rf<= 1.6])/nrow(data_1032) # % of sandeel
table(data_1032[rf>= 0.8 & rf<= 1.6]$category)


#== mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -54 & SV_mean<= -43])/nrow(data_1032)
table(data_1032[SV_mean >= -54 & SV_mean<= -43]$category)


#== frequency response(rf) + mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40 &  rf >= 0.8 & rf<= 3.5]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[Frequency==200 & category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6])/nrow(data_1032)
table(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6]$category)

data <- data.table(School_EROS.dt)
summary(10*log10(data$sV_mean))
#EROS : -102.71(Min). -12.68(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-102, -12, 1), labels=as.character(seq(-101, -12, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
data <- subset(data, Frequency %in% 200)

data_1032 <- data.table(School_1032.dt)
summary(10*log10(data_1032$sV_mean))
#1032 : -82.78(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-82, -20, 1), labels=as.character(seq(-81, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
data_1032 <- subset(data_1032, Frequency %in% 200)

#== frequency response(rf) + school size ==#
#= EROS =#
test <- data[school_area > 100 & rf >= 0.8  & rf<= 1.7]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND" ])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= saildrone =#
nrow(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7])/nrow(data_1032)
sum(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7]$sA)/sum(data_1032$sA) # sum(sA)/sum(sA from all sandeel schools)

#== frequency response(rf) + school size + mean_SV ==#
#= EROS =#
test <- data[SV_mean >= -55 & SV_mean<= -40  &  rf >= 0.7 & rf<= 1.8]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND"])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= SAILDRONE =#
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  pixelNo > 324 &  rf >= 0.9 & rf<= 1.6])/nrow(data_1032)

#== mean_SV + school size ==#
test <- data[Frequency==200 & SV_mean >= -55 & SV_mean<= -45 &  school_area > 100 ]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
#table(test$category)
nrow(test[test$category=="SAND"])/length(unique(School_EROS.dt$id[School_EROS.dt$category=="SAND"])) # exclusion % of sandeel schools
#= SAILDRONE =#


test <- subset(School_EROS.dt[,-c("id","category","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "sV_stdv","area", "SE", "YMD_time", "altitude")])
test2 <-  melt(test)
ggplot(test2, aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")



cols <- as.character(slda$finalModel$model$name)
cols <- append(cols,c("id", "category"))
data <- subset(School_EROS.dt, Frequency %in% c(200)& !category %in% "PSAND")#& !category %in% "PSAND"
data[, setdiff(names(data), cols) := NULL][]
cols_log <- cols[!cols %in% c("DepthStop","meanDepth","DepthfromBottom", "altitude_degree", "id", "category")]
data[,(cols_log):=lapply(.SD, log),.SDcols=cols_log]
#Split the data into training and test set:
training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
#Estimate pre-processing parameters
preproc.param <- train.data[,-c("id")] %>% preProcess(method = c("center", "scale"))
#Transform the data using the estimated parameters
train.data <- predict(preproc.param, train.data)
test.data <- predict(preproc.param, test.data)




#==discriminant analysis==#
#== Fit the model ==#
formula <- as.character(slda$finalModel$formula[3])
formula <- paste0("category~", formula)
model <- lda(as.formula(formula), data = train.data[,-c("id")]) #qda, mda, fda
plot(model, dimen=1, type="both") #plot of dimension 1
#library(devtools)
# Make predictions
pred.train <- predict(model, train.data)
pred.test <- predict(model, test.data)
# Model accuracy
mean(pred.train$class==train.data$category)
table(train.data$category, pred.train$class)
#mean(pred.test$class==test.data$category)
#table(test.data$category, pred.test$class)
library(ROCR)
pred <- prediction(pred.test$posterior[,2], test.data$category) # 2 class->[,2], 3 class->[,3]
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate

#library(ggord)
#ggord(model, train.data$category, xlim=c(-7, 7), ylim=c(-7.5,5))

#== determine ids "SAND" ==#
apply_SD <-  predict(slda, test_SD.data)
predict(slda, test.data, type="prob")
test_SD.data$predict <- apply_SD
nrow(test_SD.data[predict=="SAND"])/nrow(test_SD.data)
table(test_SD.data$predict)
ids <- test_SD.data[predict %in% c("SAND")]$id
#write.csv(ids, "ids_9.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#featurePlot(test_SD.data[, 3:19],as.factor(test_SD.data$predict), plot="density", auto.key = list(columns = 2))

#== "KORONA" + "manual" ==#
test_SD_KM.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% c("KORONA","manual") & !area %in% "outside")
test_SD_KM.data[,cols]=NULL
test_SD_KM.data[, Frequency:=NULL]
test_SD_KM.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+2)][,school_height:=log(school_height)]
test_SD_KM.data <- predict(preproc.param, test_SD_KM.data)
test_SD_KM.data$predictions <- predict(slda.lst[[1]], test_SD_KM.data)
table(test_SD_KM.data$predictions, test_SD_KM.data$category)
ids <- test_SD_KM.data[predictions%in%"SAND"]$id
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)




#=========================#
#### Logistic analysis ####
#=========================#
cols <- c("id","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "area", "YMD_time", "altitude", "nor_Depth", 
          "weighted_meanDepth")
#"SE","sV_stdv","school_length","school_height", "DepthStart", "DepthStop", "meanDepth" ,"sA", "altitude_degree", "Perimeter", "DepthfromBottom", "Elongation", "school_rect", "school_circ",
data <- subset(School_EROS.dt, Frequency %in% 200 & !category %in% "PSAND" & school_area >= median(School_EROS.dt$school_area))#& !category %in% "PSAND"
data[,cols]=NULL
data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
#Split the data into training and test set:
training.samples <- data$category %>%
  createDataPartition(p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. Categorical variables are automatically ignored.
# SNAD -> 1, OTHER -> 0 
train.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
test.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")


#model fit
#train.data$category <- as.factor(train.data$category)
lr <- train(as.double(category)~., data = train.data, family=binomial(), importance = TRUE,
            trControl = trainControl(method = "repeatedcv",number=10, repeats = 5, savePredictions = "final", classProbs = TRUE),
            method = "glmStepAIC")
lr$results
lr$finalModel
varImp(lr$finalModel)
plot(lr, type=c("g", "o"))

predictions.test <- ifelse(predict(lr, test.data)> 0.5, 1, 0)
mean(predictions.test==test.data$category)
confusionMatrix(reference=as.factor(test.data$category) , data= as.factor(predictions.test), mode = "everything", positive = as.character("1"))

library(ROCR)
predictions.test <- predict(slda, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate



#model <- glm(as.numeric(category)~., data=train.data, family = binomial())
train.data[, predict := predict(model, type="response")][,predict := ifelse(predict> 0.5, 1, 0)]
sum(train.data$category == train.data$predict)/nrow(train.data)
table(train.data$category, train.data$predict)
#test data
test.data[, predict := stats::predict(model, type="response", newdata = test.data)][,predict := ifelse(predict> 0.5, 1, 0)]
sum(test.data$category == test.data$predict)/nrow(test.data)
table(test.data$category, test.data$predict)
#find cutoff value using ROC



#apply to saildron data
test_SD.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% "KORONA" & !area %in% "outside")
test_SD.data[,cols]=NULL
test_SD.data[, Frequency:=NULL]
test_SD.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
ggplot(melt(test_SD.data[,-c("id", "category")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")
test_SD.data[, predict := ifelse(predict(lr, test_SD.data)> 0.5, "SAND", "OTHER")]
table(test_SD.data$predict)

ids <- test_SD.data[predict %in% "SAND"]$id
write.csv(ids, "ids_LR_1.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#




ids <- School_EROS.dt[category %in% c("SAND") & Frequency %in% 200 & !area %in% "outside"
                      #& school_area >=100
                      #& 10*log10(sV_mean) >= -55 & 10*log10(sV_mean) <= -40 
                      #& rf >= 0.8 & rf<= 3.5
                      #& area %in% c("Engelsk_Klondyke") # "Engelsk_Klondyke" / "Inner_Shoal_East_2016","Inner_Shoal_North", "Inner_Shoal_West_2018", "Inner_Shoal_test"
]$id
sandeel.dt <-  subset(School_EROS.dt, id %in% ids & Frequency %in% 200)



### horizontal distribution plot ###
data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #sandeel.dt #& school_area >= median(School_EROS.dt$school_area)
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- mutate (data, month = case_when (month=="04"~"April", month=="05"~"May", month=="06"~"June", month=="07"~"July~", TRUE ~"July~"))
data <- mutate (data, area = case_when (area=="AlbjoernLing" ~ "AlbjoernLing", area=="VestbankenSouthEast" ~ "Vestbanken",area=="VestbankenSouthWest" ~ "Vestbanken", area=="Vestbanken_North" ~ "Vestbanken",area=="Vikingbanken" ~ "Vikingbanken", area=="Engelsk_Klondyke" ~ "Engelish Klondyke",area=="Inner_Shoal_East_2016" ~ "Inner Shoal", area=="Inner_Shoal_North" ~ "Inner Shoal",area=="Inner_Shoal_West_2018" ~ "Inner Shoal", area=="Inner_Shoal_test" ~ "Inner Shoal",area=="Nordgyden" ~ "Nordgyden", area=="Ostbanken" ~ "Ostbanken", area=="Outer_Shoal" ~ "Outer Shoal",TRUE ~ "NA"))
setDT(data)[, scale_lat:= scale(Latitude)[,1], by=area][, scale_lon:=scale(Longitude)[,1], by = area ]

ggplot(data=data) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  geom_point(aes(x=scale_lat, y=log(sA)),size=1, shape=1) + # + scale_y_continuous(trans = log_trans(), breaks = trans_breaks("log", function(x) exp(x)),labels = trans_format("log", math_format(e^.x))) + 
  geom_smooth(method="lm", formula=log(y) ~ x, se=F, aes(x=scale_lat, y=sA)) + #,colour="grey", linetype = "dashed"
  #facet_grid(area~factor(month, levels=c("April", "May", "June", "July~"))) + #, ncol=1, scales = "free_x", strip.position="right"
  coord_flip() + 
  labs(x="Latitude", y="mean NASC of Sandeel (200kHz)")
#

#== center of gravity ==#
center_of_mass <- function(x,y,w){
  c(crossprod(x,w)/sum(w), crossprod(y,w)/sum(w))}
center_of_mass(data$Latitude, data$Longitude, data$sA)



#acos(sin(pi*(Latitude)/180.0)*sin(pi*(Latitudestart)/180.0)+cos(pi*(Latitude)/180.0)*cos(pi*(Latitudestart)/180.0)*cos(pi*(Longitude)/180.0-pi*(Longitudestart)/180.0))*6378
