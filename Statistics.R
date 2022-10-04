rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#================================#
####         Load data        ####
#================================#
#== choose which LDA result will be used for subsequent analysis ==#
dir <- choose.dir()
dir <- gsub('\\\\', '\\/', dir)

lapply(c("Data/School_EROS.Rdata", "Data/spdf.Rdata", "gps.Rdata", 
         paste0(dir,"/sandeel.Rdata"), paste0(dir,"/nonsandeel.Rdata")),load,.GlobalEnv)
School_EROS.dt <- School.dt
rm(School.dt)
#== add original_id and coverage_name to School_EROS.dt  ==#
School_EROS.dt[, original_id := School_EROS.dt$id]
School_EROS.dt <- School_EROS.dt[Frequency %in% 200 & category%in%"SAND"]
label <- unique(data.table(vessel = gps.dt$vessel, coverage_name = gps.dt$coverage_name, 
                           StartTime = gps.dt$StartTime, StopTime = gps.dt$StopTime,
                           distance_sum = gps.dt$distance_sum, area = gps.dt$area))
setDT(School_EROS.dt)[setDT(label), on =. (YMD_time >= StartTime, 
                                       YMD_time <= StopTime,
                                       area == area, vessel == vessel), 
                  coverage_name := coverage_name]
rm(label)

#====  (if needed)   LDA results & env.data  =========# 
lapply(c(paste0(dir,"/score.Rdata"), paste0(dir,"/coef.Rdata"), paste0(dir,"/confusion.matrix.Rdata"), 
         paste0(dir,"/slda.lst.Rdata"), paste0(dir,"/data.lst.Rdata"), paste0(dir,"/for_lift.Rdata")),load,.GlobalEnv)
load("C:/Users/a37907/Desktop/KnowSandeel15781/Data/env.Rdata")
#=====================================================#



#====  combine SD and EROS  ====#
data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #sandeel.dt #& school_area >= median(School_EROS.dt$school_area)
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- mutate (data, month_name = case_when (month=="04"~"April", month=="05"~"May", month=="06"~"June", month=="07"~"July~", TRUE ~"July~"))
data <- mutate (data, area_2 = case_when (area=="AlbjoernLing" ~ "English Klondyke", 
                                          area=="VestbankenSouthEast" ~ "Vestbanken",
                                          area=="VestbankenSouthWest" ~ "Vestbanken", 
                                          area=="Vestbanken_North" ~ "Vestbanken",
                                          area=="Vikingbanken" ~ "Vikingbanken", 
                                          area=="Engelsk_Klondyke" ~ "English Klondyke",
                                          area=="Inner_Shoal_East_2016" ~ "Inner Shoal", 
                                          area=="Inner_Shoal_North" ~ "Inner Shoal",
                                          area=="Inner_Shoal_West_2018" ~ "Inner Shoal", 
                                          area=="Inner_Shoal_test" ~ "Inner Shoal",
                                          area=="Nordgyden" ~ "Nordgyden", 
                                          area=="Ostbanken" ~ "English Klondyke", 
                                          area=="Outer_Shoal" ~ "Vestbanken",
                                          TRUE ~ "NA"))
#data[, .(cnt= sum(.N)), by= c("vessel", "area")]
#gps.dt[!coverage_name%in%c("F","x")][, .(cnt= sum(.N)), by= c("vessel", "coverage_name", "area")]

#== add coverage_name  ==# (need gps.dt)
coverage.dt <- unique(data.table(vessel=gps.dt$vessel, area=gps.dt$area, Time_start=gps.dt$StartTime, Time_end=gps.dt$StopTime, coverage_name=gps.dt$coverage_name))
coverage.dt <- coverage.dt[!coverage_name %like% c("-F")]
coverage.dt <- coverage.dt[!coverage_name %like% c("Transect")]
ggplot(coverage.dt[!coverage_name %like% c(".x")]) + 
  geom_point(aes(x=Time_start, y=coverage_name, col=vessel), 
             size=2) + 
  geom_point(aes(x=Time_end, y=coverage_name, col=vessel), 
             size=2) + 
  facet_wrap(.~vessel, scales="free", ncol=1)

#= find acoustic schools within trawl operation (position min~max) =#
setDT(data)[setDT(coverage.dt), on =. (vessel==vessel, 
                                       area==area, 
                                       YMD_time>=Time_start, 
                                       YMD_time<=Time_end), 
            coverage_name := coverage_name]
setDT(data)[, coverage_name := ifelse(is.na(coverage_name), "x", coverage_name )]

#== add coverage_name_2 ==#
setDT(data)[,time_mean:=mean(YMD_time), by=(coverage_name)]
tmp <- unique(data.table(coverage_name = data$coverage_name, time_mean = data$time_mean, area = data$area))
tmp <- tmp[order(area, time_mean),][!coverage_name %like% ".x"]
setDT(tmp)[, No:=  1:.N, by = area][, coverage_name_2:= paste(area,No,sep="-")][,area:= NULL][, time_mean:=NULL][, No :=NULL]
data <- merge(x = data, y = tmp, by = "coverage_name", all.x = TRUE)
data$coverage_name_2[is.na(data$coverage_name_2)] <- as.character("x")
rm(tmp, coverage.dt)

#==   time of day, week   ==#
data <- data |>
  mutate(time = as.numeric(strftime(YMD_time, "%H", tz="UTC"))) |>
  mutate(week = as.numeric(strftime(YMD_time, "%W", tz="UTC"))) |>
  mutate(month = strftime(YMD_time, "%m", tz="UTC")) |>
  mutate(time = case_when (time <= 04  ~ "0-4",
                           time <= 08  ~ "4-8",
                           time <= 12  ~ "8-12",
                           time <= 16  ~ "12-16",
                           time <= 20  ~ "16-20",
                           time <= 24  ~ "20-24")) |>
  mutate(week = case_when (week <= 18  ~ "Apr22-May5",
                           week <= 20  ~ "May06-May19",
                           week <= 22  ~ "May20-Jun02",
                           week <= 24  ~ "Jun03-Jun16",
                           week <= 26  ~ "Jun17-Jun30",
                           week <= 31  ~ "-Aug04"))
data$time = factor(data$time, levels = c('0-4', '4-8', '8-12',
                                         '12-16', '16-20', '20-24'), 
                   ordered = TRUE)
data$week = factor(data$week, levels = c("Apr22-May5", "May06-May19", "May20-Jun02",
                                         "Jun03-Jun16", "Jun17-Jun30", "-Aug04"), 
                   ordered = TRUE)

#== relative time to sunrise-sunset ==#
library("suncalc")
latlon <- subset(data, select=c(Latitude, Longitude, YMD_time))
colnames(latlon) <- c("lat", "lon", "date")
latlon$date <- as.Date(latlon$date, format = '%Y-%m-%d')
sun.df <- getSunlightTimes(data = latlon, keep = c("sunrise", "sunset"))
data <- cbind(data, sun.df[, c("sunrise", "sunset")])
data$day_night <- ifelse(data$YMD_time < data$sunset & data$YMD_time > data$sunrise, "day", "night")
data$hour_from_sunrise <- as.numeric(difftime(data$YMD_time, data$sunrise), units="hours")
data$sunset_sunrise <- as.numeric(difftime(data$sunset, data$sunrise), units="hours")
data$relative_time <- data$hour_from_sunrise / data$sunset_sunrise
rm(latlon, sun.df)

data <- data |>
  mutate(relative_time_2 = case_when (relative_time <= 0  ~ "before_sunrise",
                                      relative_time <= .25  ~ "0-25",
                                      relative_time <= .5 ~ "25-50",
                                      relative_time <= .75  ~ "50-75",
                                      relative_time <= 1.0  ~ "75-100",
                                      TRUE ~ "after_sunset"))
data$relative_time_2 = factor(data$relative_time_2, levels = c('before_sunrise', '0-25', '25-50',
                                                               '50-75', '75-100', 'after_sunset'), 
                              ordered = TRUE)


#== plot ==#
# NASC vs time of day
ggplot(data) + my_theme() + 
  geom_boxplot(aes(x=strftime(data$YMD_time, "%H", tz="UTC"), y=log(sA)), 
               shape=1) + 
  labs(x="Time of day", y="log(school NASC)")

# depth vs time of day
library(ggpubr)
mean <- ggplot()+theme_bw()+geom_boxplot(data=data, aes(x=strftime(data$YMD_time, "%H", tz="UTC"), y=nor_Depth*-100), shape=1)+labs(x="Time of day", y="weighted mean depth")
min <- ggplot()+theme_bw()+geom_boxplot(data=data, aes(x=strftime(data$YMD_time, "%H", tz="UTC"), y=nor_DepthStart*-100), shape=1)+labs(x="Time of day", y="minimum depth")
max <- ggplot()+theme_bw()+geom_boxplot(data=data, aes(x=strftime(data$YMD_time, "%H", tz="UTC"), y=nor_DepthStop*-100), shape=1)+labs(x="Time of day", y="maximum depth")
height <- ggplot()+theme_bw()+geom_boxplot(data=data, aes(x=strftime(data$YMD_time, "%H", tz="UTC"), y=school_height), shape=1)+labs(x="Time of day", y="school height")
ggarrange(mean, min, max, height, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) #, norm + rremove("x.text")
rm(mean, min, max, height)



# vertical
# Density change





#============================#
####    Density change    ####
#============================#
#===================================#
#==      check map gps data       ==#
#===================================#
label <- unique(data.table(vessel = gps.dt$vessel, coverage_name = gps.dt$coverage_name, 
                   StartTime = gps.dt$StartTime, StopTime = gps.dt$StopTime,
                   distance_sum = gps.dt$distance_sum, area = gps.dt$area))
spdf_tmp <-  data.table(merge(x = spdf, y = label, by = "area", all.x = TRUE))

#= plot gps and school by coverage_name =#
## "Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"
a <- c("Engelsk_Klondyke")
v <- c("SD1032")

text <- label[area %in% a & vessel %in% v]
spdf_coverage_name <- spdf_tmp[spdf_tmp$coverage_name %in% text$coverage_name ]
ggplot() + my_theme() + 
  labs(title = a) +
  geom_path(data = gps.dt[vessel %in% v & area %in% a], 
                     aes(x = Longitude, y = Latitude), lwd =.1) + 
  geom_polygon(data= spdf_coverage_name, aes(long,lat, group=area), col="black", alpha=.2) + 
  geom_text(data = text, aes(x = -Inf, y = Inf, label = StartTime), hjust   = 0, vjust   = 1) + 
  geom_text(data = text, aes(x = -Inf, y = -Inf, label = StopTime), hjust   = 0, vjust   = 0) +
  geom_point(data = sandeel.dt[area %in% a & vessel %in% v], 
             aes(x = Longitude, y = Latitude, size = log(sA)), shape = 1, col = "red") +
  facet_wrap(~coverage_name, scales="free")

#= gps and SD school by month =#
m <- as.character("06")
ggplot() + 
  geom_path(data = gps.dt[month %in% m & vessel %in% c("SD1031", "SD1032")], 
            aes(x = Longitude, y = Latitude, colour = vessel), lwd =.1) +
  geom_polygon(data= spdf, aes(long,lat, group=area), col="black", alpha=.2) + 
  geom_point(data = sandeel.dt[strftime(YMD_time, "%m", tz="UTC") %in% m], 
             aes(x = Longitude, y = Latitude), shape = 1, size = 1)
#==============================#



#=====================================#
#== calculate sA/distance, sA*area  ==#
#=====================================#

#==  calculate sA by coverage  ==# 
data[, .(cnt= sum(.N)), by= c("vessel", "coverage_name_2", "area")]
setDT(data)[, sA_sum:= sum(sA), by=c("coverage_name", "vessel")]
data[label, on = 'coverage_name', distance_sum := distance_sum]
data$sA_coverage <- data$sA_sum/data$distance_sum
data[ ,YMD_time_mean := mean(YMD_time), by=c("coverage_name", "vessel")][, sA_sum := NULL]
library(lubridate)
data[ ,Julian_day_mean := yday(YMD_time_mean)]

#==  calculate sA_coverage by day  ==#
library(lubridate)
data[ ,Julian_day := yday(YMD_time)][, hour := strftime(YMD_time, format = "%H")]
data[, sA_sum_daily:= sum(sA), by=c("coverage_name", "vessel", "Julian_day")]

gps.dt <- gps.dt[order(vessel, YMD_time),]
gps.dt[, Julian_day := yday(YMD_time)]
gps.dt[, distance_sum_daily := sum(distance), by=c("coverage_name", "vessel", "Julian_day")]
gps.dt[, hour := strftime(YMD_time, format = "%H")][, distance_sum_hourly := sum(distance), by=c("coverage_name", "vessel", "Julian_day", "hour")]

data[gps.dt, on = c('coverage_name', "vessel", "Julian_day"), 
     distance_sum_daily := distance_sum_daily]
data[gps.dt, on = c('coverage_name', "vessel", "Julian_day", "hour"), 
     distance_sum_hourly := distance_sum_hourly]
data[, sA_coverage_daily := sA_sum_daily/distance_sum_daily][, sA_sum_daily := NULL]



#==  calculate sA*area by day and hour  ==#
data[, sA_area := sA * school_area]
data[, sA_area_sum := sum(sA_area), by=c("coverage_name", "vessel", "Julian_day")]
data[, sA_area_daily := sA_area_sum / distance_sum_daily, by=c("coverage_name", "vessel")][, sA_area_sum := NULL ]
data[, sA_area_sum_hourly := sum(sA_area), by=c("coverage_name", "vessel", "Julian_day", "hour")]
data[, sA_area_hourly := sA_area_sum_hourly / distance_sum_hourly, by=c("coverage_name", "vessel",  "Julian_day", "hour")][, sA_area_sum_hourly := NULL]
#=========================================#






#== grid data  ==#

makeGridDt <- function (nmi = 1, gpsdata, schooldata) {
  # make distance group
  gps.dt <- data.table(gpsdata[order(vessel, YMD_time),])
  gps.dt[, distance_cum := cumsum(distance), by = vessel][, distance_cum_nmi := distance_cum / (1852 * nmi)]
  gps.dt[, distance_group := as.factor(floor(distance_cum_nmi))]
  gps.dt[, distance_sum_nmi := sum(distance), by=c("vessel", "distance_group")]
  
  # make grid data table
  grid.dt <- gps.dt[ , .(YMD_time_min = min(YMD_time), YMD_time_max = max(YMD_time), 
                         Latitudemin = min(Latitude), Latitudemax = max(Latitude), Latitudemean = mean(Latitude), 
                         Longitudemin = min(Longitude), Longitudemax = max(Longitude), Longitudemean = mean(Longitude), 
                         distance_sum_nmi = distance_sum_nmi), 
                     by = .(vessel, distance_group)] 
  grid.dt <- unique(grid.dt)
  
  # calculate mean time, lat, lon
  grid.dt[, diff_time := c(0, diff(as.numeric(YMD_time_max), units = "secs")), by = c("vessel")]
  grid.dt[, YMD_time_min2 := as.POSIXct(as.numeric(YMD_time_max)-(diff_time-1), origin = "1970-01-01", tz = "UTC"), by = c("vessel")]
  grid.dt[, YMD_time_min := c(min(YMD_time_min), YMD_time_min2[-1]), by = c("vessel")][, YMD_time_min2 := NULL][, diff_time := NULL]
  grid.dt <- grid.dt |>
    mutate(Latitudemean = Latitudemin + ((Latitudemax - Latitudemin)/2))|>
    mutate(Longitudemean = Longitudemin + ((Longitudemax - Longitudemin)/2))|>
    mutate(YMD_time_mean = as.POSIXct(as.numeric(YMD_time_min) + 
                                        ((as.numeric(YMD_time_max)-as.numeric(YMD_time_min))/2), 
                                      origin = "1970-01-01", tz = "UTC"))
  
  # set Date, time of day, week, Julian day
  grid.dt <- grid.dt |>
    mutate(Date = as.Date(YMD_time_mean)) |>
    #mutate(time = as.numeric(strftime(YMD_time_mean, "%H", tz="UTC"))) |>
    mutate(week = as.numeric(strftime(YMD_time_mean, "%W", tz="UTC"))) |>
    mutate(month = strftime(YMD_time_mean, "%m", tz="UTC")) |>
    #mutate(time = case_when (time <= 04  ~ "0-4",
    #                         time <= 08  ~ "4-8",
    #                         time <= 12  ~ "8-12",
    #                         time <= 16  ~ "12-16",
    #                         time <= 20  ~ "16-20",
    #                         time <= 24  ~ "20-24")) |>
    #mutate(week = case_when (week <= 18  ~ "Apr22-May5",
    #                         week <= 20  ~ "May06-May19",
    #                         week <= 22  ~ "May20-Jun02",
    #                         week <= 24  ~ "Jun03-Jun16",
    #                         week <= 26  ~ "Jun17-Jun30",
    #                         week <= 28  ~ "Jul01-Jul14",
    #                         week <= 31  ~ "-Aug04",
    #                         week <= 33  ~ "-Aug18"))|>
    #mutate(time = factor(time, levels = c('0-4', '4-8', '8-12','12-16', '16-20', '20-24'), 
    #                     ordered = TRUE)) |>
    #mutate(week = factor(week, levels = c("Apr22-May5", "May06-May19", "May20-Jun02",
    #                                      "Jun03-Jun16", "Jun17-Jun30","Jul01-Jul14", 
    #                                      "-Aug04","-Aug18"), 
    #                     ordered = TRUE))|>
    mutate(Julian_day = lubridate::yday(YMD_time_mean))
    
  # relative time to sunrise-sunset
  latlon <- subset(grid.dt, select=c(Latitudemean, Longitudemean, YMD_time_mean)) |>
    mutate(date = as.Date(YMD_time_mean, format = '%Y-%m-%d'))|>
    rename(lat = Latitudemean, lon = Longitudemean)|>
    select(-c(YMD_time_mean))
  
  sun.df <- suncalc::getSunlightTimes(data = latlon, keep = c("sunrise", "sunset"))
  grid.dt <- cbind(grid.dt, sun.df[, c("sunrise", "sunset")])
  
  grid.dt <- grid.dt |>
    #mutate(day_night = ifelse(YMD_time_max < sunset & YMD_time_min > sunrise, "day", "night")) |>
    mutate(hour_from_sunrise = as.numeric(difftime(YMD_time_mean, sunrise), units="hours"))|>
    mutate(sunset_sunrise = as.numeric(difftime(sunset, sunrise), units="hours")) |>
    mutate(relative_time = hour_from_sunrise / sunset_sunrise)|>
    #mutate(relative_time_2 = case_when (relative_time <= 0  ~ "before_sunrise",
    #                                    relative_time <= .25  ~ "0-25",
    #                                    relative_time <= .5 ~ "25-50",
    #                                    relative_time <= .75  ~ "50-75",
    #                                    relative_time <= 1.0  ~ "75-100",
    #                                    TRUE ~ "after_sunset"))|>
    #mutate(relative_time_2 = factor(relative_time_2, 
    #                                levels = c('before_sunrise', '0-25', '25-50','50-75', '75-100', 'after_sunset'), 
    #                                ordered = TRUE))
    select(-c("sunrise", "sunset", "hour_from_sunrise",  "sunset_sunrise"))
  
  
  # sA from school data (sandeel.dt / School_EROS.dt)
  #school.dt <- data.table(rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND"]))|>
  school.dt <- data.table(schooldata)|>
    mutate(floor_date= lubridate::floor_date(YMD_time))
  
  setDT(school.dt)[setDT(grid.dt), on =. (floor_date >= YMD_time_min, 
                                     floor_date <= YMD_time_max, 
                                     vessel == vessel),
              distance_group := distance_group]
  
  setDT(school.dt)[setDT(grid.dt), on =. (distance_group == distance_group, 
                                     vessel == vessel),
              distance_sum_nmi := distance_sum_nmi]
  
  school.dt <- school.dt |>
    group_by(vessel, distance_group)|>
    mutate(sA.area_nmi = sum(sA * school_area) / distance_sum_nmi) |>
    mutate(sA_nmi = sum(sA) / distance_sum_nmi) |>
    mutate(school_no = n()) |>
    select(c("vessel", "distance_group", "sA_nmi", "sA.area_nmi", "school_no", "area", "coverage_name"))
  
  
  
  #grid.dt <- data.table(grid.dt)
  #school.dt <- data.table(school.dt)
  
  grid.dt <- grid.dt |>
    left_join(school.dt, by = c("vessel", "distance_group"))

  #setDT(grid.dt)[setDT(school.dt), on =. (distance_group == distance_group, 
  #                                   vessel == vessel),
  #               c("sA_nmi", "sA.area_nmi", "school_no", "area", "coverage_name") 
  #               := .(sA_nmi, sA.area_nmi, school_no, area, coverage_name)] 
  
  
  # fill NA values
  area.confirm <- data.table(with(gps.dt, aggregate(gps.dt[,c("area","coverage_name")], 
                                                    list(vessel, distance_group), 
                                                    function(x) names(which.max(table(x))))) )
  area.confirm <- area.confirm[order(Group.1, Group.2),]
  area.confirm <- area.confirm |>
    rename(area.confirm = area, 
           coverage_name.confirm = coverage_name, 
           vessel = Group.1, 
           distance_group = Group.2)
  
  grid.dt <- grid.dt |>
    left_join(area.confirm, by = c("vessel", "distance_group"))|>
    mutate(area = ifelse(is.na(area), area.confirm, area))|>
    mutate(coverage_name = ifelse(is.na(coverage_name), coverage_name.confirm, coverage_name))|>
    select(-c("area.confirm",  "coverage_name.confirm"))|> 
    mutate(sA_nmi = ifelse(is.na(sA_nmi), as.numeric(0), sA_nmi)) |>
    mutate(sA.area_nmi = ifelse(is.na(sA.area_nmi), as.numeric(0), sA.area_nmi)) |>
    mutate(school_no = ifelse(is.na(school_no), as.numeric(0), school_no))
    #mutate_at(vars(contains(colname_sA_nmi)), tidyr::replace_na, 0)
  
  # set larger area
  grid.dt <- grid.dt |>
  mutate (area_2 = case_when (area == "AlbjoernLing"|area == "Engelsk_Klondyke"|area == "Ostbanken" 
                              ~ "English Klondyke",
                              area == "VestbankenSouthEast" |area == "VestbankenSouthWest" |area == 
                                "Vestbanken_North" |area == "Outer_Shoal" 
                              ~ "Vestbanken",
                              area == "Inner_Shoal_East_2016" |area == "Inner_Shoal_North" |area == 
                                "Inner_Shoal_West_2018" |area == "Inner_Shoal_test" 
                              ~ "Inner Shoal",
                              area == "Vikingbanken" ~ "Vikingbanken",
                              area == "Nordgyden" ~ "Nordgyden",
                              TRUE ~ "outside"))
  
  
  return(grid.dt)
}


grid.dt <- makeGridDt(nmi = 10, gpsdata = gps.dt[!vessel %in% "EROS"], schooldata = sandeel.dt)


#=== grid data plot ===#

makePlotfromGridDt_point <- function (data, x, y, colour, facetVar = NULL, axis.y.name = NULL, model = NULL) { 
  
  #ymin <- ifelse(grepl("log", y), log(min(data[[gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", y, perl=T)]])), min(data[[y]]))
  
  #ymax <- ifelse(grepl("log", y), log(max(data[[gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", y, perl=T)]])), max(data[[y]]))
    
  
  y_lab <- if (!is.null(axis.y.name)) ylab(axis.y.name)

  
  facet_layer <- if (!rlang::quo_is_null(rlang::enquo(facetVar))) facet_grid(formula(paste("~", facetVar)))
  
  model_layer <- if(!rlang::quo_is_null(rlang::enquo(model)))  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "black")
  
  plot <- ggplot(data = data[!vessel %in% "EROS"], 
                 aes_string(x = x, y = y, fill = colour)) + 
    my_theme() + 
    theme(axis.title.x = element_blank()) +
    geom_point(shape = 21) + 
    xlim(min(data[[x]]), max(data[[x]])) +
    #ylim(ymin, ymax) +
    y_lab + 
    facet_layer + 
    model_layer
  
  #geom_histogram(aes(log(sA_1nmi)), bins = 30, col = 'grey50', fill = 'transparent', alpha=0.3) + coord_flip()
  
  return(plot)
  
}



makePlotfromGridDt_point(data = grid.dt[!vessel %in% "EROS"], x = "Julian_day", y = "log(school_no + 1)", colour = "area_2", facetVar = NULL, axis.y.name = "log(number of school)")
#


#=== STATS sA_nmi vs time ===#
mod1 <- gam(log(sA_nmi+1) ~ Julian_day, data = grid.dt)
mod2 <- gam(log(sA_nmi+1) ~ s(Julian_day), data = grid.dt)


ggplot(grid.dt[!vessel %in% "EROS"], aes(x = relative_time, y = log(sA_nmi+1))) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),col = "black") + 
  #geom_line(aes(y=predict(mod2))) + 
  my_theme()


 
  


#=========================================#

#=======================#
#==     plot data     ==#
#=======================#
## "Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"
a <- c("Engelsk_Klondyke")
library(ggrepel)

#= sA_coverage by coverage
data |>
  arrange(YMD_time) |> filter(area %in% a) |>
  #filter(vessel != "EROS" ) |> 
  #filter(!coverage_name %like% ".x") |>
  ggplot(aes(x = YMD_time_mean, y = log(sA_coverage), fill=vessel)) +
  geom_point(shape = 21) +
  #geom_path(aes(group = interaction(vessel, area))) +
  #geom_point(data = data[!vessel %in% "EROS" & coverage_name %like% ".x"], 
  #           aes(x = YMD_time_mean, y = log(sA_coverage)), fill = "white", shape = 21) +
  #geom_label_repel(data = data[!vessel %in% "EROS" & !duplicated(data[,c('coverage_name')]),],
  #                 aes(x = YMD_time_mean, y = log(sA_coverage), label = area),
  #                 box.padding   = .5, label.padding = .15, point.padding = .1, 
  #                 force = 10, max.overlaps = length(unique(data$coverage_name)), 
  #                 min.segment.length = .1 ,size = 2) + 
  xlim(min(data$YMD_time_mean), max(data$YMD_time_mean)) + 
  ylim(c(min(log(data$sA_coverage)), max(log(data$sA_coverage)))) + 
  #labs(y="log(daily total school NASC)", x = "") + 
  my_theme() + theme(legend.position = "none") 

#= sA_coverage by day
data |>
  arrange(YMD_time) |> filter(area %in% a) |>
  #filter(vessel != "EROS" ) |> 
  #filter(!coverage_name %like% ".x") |>
  ggplot(aes(x = Julian_day, y = log(sA_coverage_daily), fill=vessel)) +
  geom_point(shape = 21) +
  #geom_path(aes(group = interaction(vessel, area))) +
  #geom_point(data = data[!vessel %in% "EROS" & coverage_name %like% ".x"], 
  #           aes(x = YMD_time_mean, y = log(sA_coverage)), fill = "white", shape = 21) +
  #geom_label_repel(data = data[!vessel %in% "EROS" & !duplicated(data[,c('coverage_name')]),],
  #                 aes(x = YMD_time_mean, y = log(sA_coverage), label = area),
  #                 box.padding   = .5, label.padding = .15, point.padding = .1, 
  #                 force = 10, max.overlaps = length(unique(data$coverage_name)), 
  #                 min.segment.length = .1 ,size = 2) + 
  xlim(c(min(data$Julian_day), max(data$Julian_day))) + 
  ylim(c(min(log(data$sA_coverage_daily)), max(log(data$sA_coverage_daily)))) +
  #facet_grid(area~., scales = "free") +
  #labs(y="log(daily total school NASC)", x = "") + 
  my_theme() + theme(legend.position = "none") 


#= sA
data |>
  arrange(YMD_time) |> filter(area %in% a) |>
  #filter(vessel != "EROS" ) |> 
  #filter(!coverage_name %like% ".x") |>
  ggplot(aes(x = YMD_time, y = log(sA), fill=vessel)) +
  geom_point(shape = 21) +
  #geom_path(aes(group = interaction(vessel, area))) +
  #geom_point(data = data[!vessel %in% "EROS" & coverage_name %like% ".x"], 
  #           aes(x = YMD_time_mean, y = log(sA_coverage)), fill = "white", shape = 21) +
  xlim(min(data$YMD_time), max(data$YMD_time)) + ylim(c(min(log(data$sA)), max(log(data$sA)))) + 
  #facet_grid(area~., scales = "free") +
  #labs(y="log(daily total school NASC)", x = "") + 
  my_theme() + theme(legend.position = "none") 


#= sA by day
data |>
  arrange(YMD_time) |> filter(!area %in% a) |>
  #filter(vessel != "EROS" ) |> 
  #filter(!coverage_name %like% ".x") |>
  ggplot(aes(x = Julian_day, y = log(sA_area_daily), fill=vessel)) +
  geom_point(shape = 21) +
  #geom_path(aes(group = interaction(vessel, area))) +
  #geom_point(data = data[!vessel %in% "EROS" & coverage_name %like% ".x"], 
  #           aes(x = YMD_time_mean, y = log(sA_coverage)), fill = "white", shape = 21) +
  #geom_label_repel(data = data[!vessel %in% "EROS" & !duplicated(data[,c('coverage_name')]),],
  #                 aes(x = YMD_time_mean, y = log(sA_coverage), label = area),
  #                 box.padding   = .5, label.padding = .15, point.padding = .1, 
  #                 force = 10, max.overlaps = length(unique(data$coverage_name)), 
  #                 min.segment.length = .1 ,size = 2) + 
  xlim(c(min(data$Julian_day), max(data$Julian_day))) + 
  ylim(c(min(log(data$sA_area_daily)), max(log(data$sA_area_daily)))) +
  #labs(y="log(daily total school NASC)", x = "") + 
  my_theme() + theme(legend.position = "none") 


#=== sA(density) histogram by f.ground  ==#
# count how many coverage is in a fishing ground
aggregate(data = data[!coverage_name %like% ".x"], coverage_name ~ area, 
          function(x) length(unique(x)))
#Engelsk_Klondyke(6), Inner_Shoal_East_2016(5), Inner_Shoal_West_2018(4), 
#Outer_Shoal(3), Vestbanken_North(3), VestbankenSouthWest(4), 
A <- data[area %in% "AlbjoernLing"]
A[, date_mean := mean(YMD_time), by = c("coverage_name")][, date_mean_chr := format(date_mean, format="%m-%d")]
A <- A[order(date_mean),]
A[, tmp := length(unique(coverage_name)), by = date_mean_chr]
A[, date_mean := ifelse(tmp != 1, 
                        format(date_mean, format="%m-%d-%H"), 
                        format(date_mean, format="%m-%d"))][, tmp:= NULL][, date_mean_chr := NULL]
A[, date_mean := ifelse(grepl(".x", coverage_name), paste0(date_mean,".x"), date_mean)]
#aggregate(YMD_time ~ coverage_name, data = A, FUN = mean)
mean <- data.table(aggregate(log(sA_area) ~ date_mean + coverage_name, data = A, FUN = 'mean'))
colnames(mean) <- c("date_mean", "coverage_name", "sA_mean")
ggplot(data = A, #[!coverage_name %like% ".x"]
         aes(fill = vessel, log(sA_area))) + 
  my_theme() + 
  geom_histogram(binwidth = .5, col = 'grey50',  alpha=0.3) + #fill = 'transparent',
  geom_vline(data = mean, #[!coverage_name %like% ".x"]
             aes(xintercept = sA_mean), colour='red') + 
  facet_grid(.~date_mean) +
  labs(title = unique(A$area)) + 
  coord_flip()
#

# areas being visited over 6 different times
a <- c("Nordgyden", "Vikingbanken") 
# visited 6 times : "Engelsk_Klondyke", "Inner_Shoal_East_2016", "VestbankenSouthWest"
# visited 5 times : "Outer_Shoal", "VestbankenSouthEast"
# visited 4 times : "AlbjoernLing", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
# visited 3 times : "Vestbanken_North"
# visited 2 times : "Ostbanken"
# visited 1 time  : "Nordgyden", "Vikingbanken"
A <- data#[!area %in% a]
A[, date_mean := mean(YMD_time), by = c("coverage_name", "area")][, date_mean_chr := format(date_mean, format="%m-%d")]
A <- A[order(area, date_mean),]
A[, sA_mean := mean(log(sA)), by = c("area", "date_mean", "coverage_name")]
A[, sA_SE := sd(log(sA)) / sqrt(length(log(sA))), by = c("area", "date_mean", "coverage_name")]
A[, sA.area_mean := mean(log(sA_area)), by = c("area", "date_mean", "coverage_name")]
A[, sA.area_SE := sd(log(sA_area)) / sqrt(length(log(sA_area))), by = c("area", "date_mean", "coverage_name")]
A[, area_mean := mean(log(school_area)), by = c("area", "date_mean", "coverage_name")]
A[, area_SE := sd(log(school_area)) / sqrt(length(log(school_area))), by = c("area", "date_mean", "coverage_name")]


ggplot(A[!vessel %in% "EROS"], aes(x = date_mean, y = sA_mean)) + 
  geom_errorbar(aes(ymin = sA_mean - sA_SE, ymax = sA_mean + sA_SE), width=.1) +
  #geom_line(aes(linetype = area)) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) + 
  xlim(as.POSIXct("2019-04-24", tz = "UTC"), as.POSIXct("2019-08-08", tz = "UTC")) + 
  my_theme()



#=========================================#






#=======================#
####       GAM       ####
#=======================#
library(mgcv)
#=========================#
#==    data rearrange   ==#
#==  extract only mean  ==#
#=========================#
a <- data.table(aggregate(data = data[!coverage_name %like% ".x"], coverage_name ~ area, 
                          function(x) length(unique(x))))
a <- a[coverage_name >= 3]
a <- as.character(a$area)


#== by coverage ==#
t <- data.table(with(data, aggregate(data[,c("YMD_time","Julian_day")], 
                                     list(area_2, area, coverage_name, vessel), mean)))
m <- data.table(with(data, aggregate(data[,c("sA", "sA_area", "school_area", "sV_mean", "sA_coverage")], 
                                     list(area_2, area, coverage_name, vessel), function(x) mean(log(x)))))
m <- m |> rename(area_2 = Group.1, area = Group.2, coverage_name = Group.3, vessel = Group.4,
                 sA_mean = sA, sA.area_mean = sA_area, area_mean = school_area)
se <- data.table(with(data, aggregate(data[,c("sA", "sA_area", "school_area", "sV_mean")], 
                                      list(area_2, area, coverage_name, vessel), function(x) sd(log(x)) / sqrt(length(log(x))))))
se <- se |> rename(sA_SE = sA, sA.area_SE = sA_area, area_SE = school_area, sV_SE = sV_mean)
coverage <- data.table(cbind(t[,-c("Group.1", "Group.2", "Group.3", "Group.4")], 
                             m, 
                             se[,-c("Group.1", "Group.2", "Group.3", "Group.4")]))
coverage <- coverage |> mutate_if(is.character, as.factor)
rm(t, m, se)

#==    model & plot     ==#
mod <- gam(sA_mean ~ s(Julian_day), data = coverage)
mod2 <- gam(sA_mean ~ area_2 + s(Julian_day), data = coverage)
# randome effect
mod3 <- gamm(sA_mean ~ s(Julian_day), data = coverage, random = list(area_2=~1)) # with mixed effect (random intercept, common slope) 
mod4 <- gam(sA_mean ~ s(Julian_day) + s(area_2, bs = "re"), data = coverage, method = "REML")
# ordinary linear model
mod_lm <- gam(sA_mean ~ Julian_day, data = coverage)

plot.gam(mod, residuals=T, pch = 20, pages = 1) #fitted vs. residuals, should be horizontal and evenly spread residuals
gam.check(mod, pages = 1) #diagnostic plot, Residuals vs linear pred should have points cloud around 0
library(itsadug)
plot_smooth(mod4, view = "Julian_day", rm.ranef = TRUE, main = "intercept + s(area_2)")
plot_smooth(mod4, view = "Julian_day", rm.ranef = FALSE, cond = list(fac = "Inner Shoal"),
            main = "... + s(area_2)", col = "orange", ylim = c(3, 11))

anova(mod, mod_lm, test = "Chisq")
summary(mod)
summary(mod3[[2]])
ggplot(cbind(coverage, fit1 = fitted(mod2)), 
       aes(x = YMD_time, y = sA_mean)) + 
  geom_errorbar(aes(ymin = sA_mean - sA_SE, ymax = sA_mean + sA_SE), width=.1) +
  #geom_line(aes(linetype = area)) +
  geom_point(aes(fill = area_2), shape = 21, size = 2) + 
  geom_line(aes(y = fit1), colour = "black", lwd = 1) +
  xlim(as.POSIXct("2019-04-24", tz = "UTC"), as.POSIXct("2019-08-08", tz = "UTC")) + 
  my_theme()
ggplot(coverage[!vessel %in% "EROS"]) + aes(x = YMD_time, y = sA_mean) +
  geom_errorbar(aes(ymin = sA_mean - sA_SE, ymax = sA_mean + sA_SE), width=.1) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) +
  #GAM with a cubic spline(cs)
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),col = "black") +
  xlim(as.POSIXct("2019-04-24", tz = "UTC"), as.POSIXct("2019-08-08", tz = "UTC")) + 
  my_theme()



#== by daily ==#
daily <- data.table(sA_coverage_daily = data$sA_coverage_daily, sA_area_daily = data$sA_area_daily,
                Julian_day = data$Julian_day, coverage_name = data$coverage_name,
                area = data$area, area_2 = data$area_2, vessel = data$vessel)
daily <- daily[!duplicated(daily)]
daily <- daily |> mutate_if(is.character, as.factor)

#==    model & plot     ==#
mod <- gam(log(sA_coverage_daily) ~ s(Julian_day), data=daily)
mod_lm <- gam(log(sA_coverage_daily) ~ Julian_day, data=daily)
#plot.gam(mod, pages = 1)
anova(mod, mod_lm, test = "Chisq")
summary(mod)
ggplot(cbind(daily, fit1 = fitted(mod)), aes(x = Julian_day, y = log(sA_coverage_daily))) + 
  #geom_errorbar(aes(ymin = sA_coverage_daily - SE, ymax = sA_coverage_daily + SE), width=.1) +
  #geom_line(aes(linetype = area)) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) + 
  geom_line(aes(y = fit1), colour = "red", lwd = 2) +
  xlim(min(daily$Julian_day), max(daily$Julian_day)) + 
  my_theme()
ggplot(daily) + aes(x = Julian_day, y = log(sA_coverage_daily)) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) +
  #GAM with a cubic spline(cs)
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),col = "red") +
  labs(y = "daily sA_mean")+
  my_theme()

# if CI to be dashed-line
p <- ggplot(daily) + aes(x = Julian_day, y = log(sA_coverage_daily)) + 
  geom_point(aes(fill = vessel), shape = 21, size = 2) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "black", fill = "transparent") + 
  my_theme()
ggg <- ggplot_build(p)
dat <- ggg$data[[2]]
p + geom_line(aes(x, ymin), data=dat, linetype="dashed") + 
  geom_line(aes(x, ymax), data=dat, linetype="dashed")
rm(ggg, dat)



#==   by nmi model within day    ==#
mod <- gam(log(sA_nmi) ~ s(relative_time), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
ggplot(grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0]) + aes(x = relative_time, y = log(sA_nmi)) +
  geom_point(aes(fill = vessel), shape = 21, size = 2) +
  #GAM with a cubic spline(cs)
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),col = "black") +
  my_theme()
mod2 <- gam(log(sA_nmi) ~ area_2 + s(relative_time, by = area_2), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
# random effect
#mod3 <- gamm(log(sA_nmi) ~ s(relative_time), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0], random = list(area_2=~1)) # with mixed effect (random intercept, common slope) 
mod4 <- gam(log(sA_nmi) ~ s(relative_time) + s(area_2, bs = "re"), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0], method = "REML")
# ordinary linear model
mod_lm <- gam(log(sA_nmi) ~ relative_time, data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
anova(mod, mod2, mod4, mod_lm, test = "Chisq")


#==   by nmi model over the season    ==#
mod <- gam(log(sA_nmi) ~ s(unclass(YMD_time_mean)), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
ggplot(grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0]) + aes(x = unclass(YMD_time_mean), y = log(sA_nmi)) +
  geom_point(aes(fill = area_2), shape = 21, size = 2) +
  #GAM with a cubic spline(cs)
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),col = "black") +
  my_theme()
mod2 <- gam(log(sA_nmi) ~ area_2 + s(unclass(YMD_time_mean), by = area_2), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
# random effect
#mod3 <- gamm(log(sA_nmi+1) ~ s(YMD_time_mean), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0], random = list(area_2=~1)) # with mixed effect (random intercept, common slope) 
mod4 <- gam(log(sA_nmi) ~ s(unclass(YMD_time_mean)) + s(area_2, bs = "re"), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0], method = "REML")
# ordinary linear model
mod_lm <- gam(log(sA_nmi) ~ unclass(YMD_time_mean), data = grid.dt[!vessel %in% "EROS" & !sA_nmi %in% 0])
anova(mod, mod2, mod4, mod_lm, test = "Chisq")
#=========================================#











#=========================#
####     vertical      ####
#=========================#
#== visualize all school depth vs time ==#
data |>
  filter(vessel != "EROS") |>
  mutate(time = strftime(YMD_time, "%H:%M", tz="UTC")) |>
  mutate(time = as.POSIXct(time, format = "%H:%M")) |>
  ggplot() + my_theme() +
  geom_point(aes(y = nor_Depth*-1, 
                 x = time, # altitude_degree (if use altitude, comment out "scale_x_datetime()")
                 colour = vessel),
             shape = 1, alpha = 1) +
  scale_x_datetime(date_labels="%H") +
  #facet_wrap(~strftime(YMD_time, "%m", tz="UTC")) + 
  #facet_wrap(~strftime(YMD_time, "%W", tz="UTC")) + 
  labs(y = "normalised depth", x = "time of day")


l <- c("April", "May", "June", "July", "August")
names(l) <- c("04", "05", "06", "07", "08")

## "Vestbanken","English Klondyke", "Vikingbanken" 
## "Inner Shoal", "Nordgyden"
a = c("English Klondyke")

ggplot() +
  my_theme() + theme(axis.title.x = element_blank()) + 
  geom_boxplot(data = data[area_2%in%a], aes(x = month, y = meanDepth, fill = vessel)) + 
  scale_y_reverse() + 
  scale_x_discrete(labels = l) +
  labs(y="Depth of school (m)", title = a)
#

ggplot() +
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle=90)) + 
  geom_boxplot(data = data[!is.na(coverage_name_2)], aes(x=coverage_name_2, y=nor_Depth, fill=vessel)) + scale_y_reverse() + 
  labs(y="Depth of school (m)")
#





# histogram
ggplot(data[!vessel %in% "EROS" ]) + #& !month %in% c("07", "08")
  my_theme() + theme(axis.title.x = element_blank()) +
  geom_histogram(aes(nor_Depth), binwidth = .025, col = 'grey50', fill = 'transparent', alpha=0.3) + 
  scale_x_reverse() +
  labs(x="Normarised depth (m)") +
  #facet_grid(week ~ relative_time) +
  facet_grid(.~ relative_time) +
  coord_flip()


# weighted histogram
ggplot(data[!vessel %in% "EROS" ]) + #& !month %in% c("07", "08")
  geom_histogram(aes(x = nor_Depth, y = ..density.., weight = log(sA)),
                 binwidth = .025, col = 'grey50', fill = 'transparent', alpha=0.3) + 
  scale_x_reverse() +
  labs(x="Normarised depth (m)") +
  #facet_grid(week ~ relative_time_2) +
  facet_grid(.~ relative_time_2) +
  coord_flip() + my_theme()


# density plot
ggplot(data[!vessel %in% "EROS" & !month %in% c("07", "08")]) + 
  my_theme() + 
  geom_histogram(aes(nor_Depth, y = stat(density)), binwidth = .025, col = 'grey50', fill = 'transparent', alpha=0.3) + 
  geom_density(aes(nor_Depth), col = 'red', alpha=0.5) +
  scale_x_reverse() +
  labs(x="Normarised depth (m)") +
  coord_flip() + 
  facet_grid(month~relative_time_2, scales = "free_x")

#============================#























































































































#========================================================================================================================#
#### test code    ####  
#========================================================================================================================#

library(plotrix)
weighted.hist(data2$nor_Depth, data2$sA)

#===========================#
####      horizontal     ####
#===========================#

#==================================================#
####   center of gravity (local distribution)   ####
#==================================================#

#== calculate center of gravity ==#
setDT(data)[, center_of_gravity_Lat:= crossprod(Latitude,sA)/sum(sA), by=(coverage_name)][, center_of_gravity_Long:= crossprod(Longitude,sA)/sum(sA), by=(coverage_name)]

#

## "Engelsk_Klondyke", "Vikingbanken","AlbjoernLing", "Ostbanken", "Nordgyden", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"
a <- c("AlbjoernLing", "Ostbanken")
ggplot() + theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank()) + 
  geom_point(data=gps.dt[area%in%a & !coverage_name %in% c("F","x", "Transect")], aes(x=Longitude, y=Latitude),col="grey", size=.05, alpha =0.2) +
  geom_point(data=data[area%in%a & !coverage_name %in% c("F","x", "Transect")], aes(Longitude, Latitude, size = sA), alpha = 0.5)+ 
  scale_size_continuous(limits = c(0, 439373.7), range = c(0.1,20), breaks = c(500,5000, 10000, 50000)) + 
  geom_point(data=data[area %in% a & !coverage_name %in% c("F","x", "Transect")], aes(center_of_gravity_Long,center_of_gravity_Lat), col="red", size=5) + 
  facet_wrap(~coverage_name, scales="free")
#  
#

setDT(data)[,sA_sum:=sum(sA), by=c("coverage_name", "vessel")]
center_of_gravity <- with(data, aggregate(data[,c("Latitude", "Longitude", "YMD_time", "DepthStart", "DepthStop","sV_mean","rf","school_length", "meanDepth", "weighted_meanDepth","nor_Depth","nor_DepthStart", "nor_DepthStop", "DepthfromBottom")], 
                                          list(vessel, sA_sum, coverage_name, center_of_gravity_Lat,  center_of_gravity_Long), mean))
setnames(center_of_gravity, c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"), 
         c("vessel", "sA_sum", "coverage_name", "center_of_gravity_Lat",  "center_of_gravity_Long"))
setDT(center_of_gravity)[, area:=sub(".*- *(.*?) *-.*", "\\1", center_of_gravity$coverage_name)]
center_of_gravity <- center_of_gravity[order(area, YMD_time),]
setDT(center_of_gravity)[, No:= 1:.N, by=area][, coverage_name_2:=paste(area,No,sep="-")]#[,No:=NULL]
setDT(center_of_gravity)[, relative_Lat:=center_of_gravity_Lat-center_of_gravity_Lat[No==1], by = (area)][, relative_Long:=center_of_gravity_Long-center_of_gravity_Long[No==1], by = (area)]
center_of_gravity <- setDT(center_of_gravity)[, if (.N>1) .SD, by=.(area)]

ggplot() + theme_bw() + theme() +
  geom_path(data=center_of_gravity[!area%in%"x"], aes(x=relative_Long, y=relative_Lat, colour=area))+
  geom_point(data=center_of_gravity[!area%in%"x"], aes(x=relative_Long, y=relative_Lat, colour=area)) + 
  facet_wrap(~area)




#== bubble plot ==#
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("rnaturalearthhires")
library("ggspatial") #for scale bar

# make figure #
#startpoint <- data03_EK$EK_38.df %>% group_by(Mission) %>% filter(row_number()==1)
w <- ne_countries(scale = "medium", returnclass = "sf")
spdf04 <- data.frame(long=spdf$long, lat=spdf$lat, area=spdf$area, month="04")
spdf05 <- data.frame(long=spdf$long, lat=spdf$lat, area=spdf$area, month="05")
spdf06 <- data.frame(long=spdf$long, lat=spdf$lat, area=spdf$area, month="06")
spdf07 <- data.frame(long=spdf$long, lat=spdf$lat, area=spdf$area, month="07")
spdf08 <- data.frame(long=spdf$long, lat=spdf$lat, area=spdf$area, month="08")
spdf_month <- data.table(rbind(spdf04, spdf05, spdf06, spdf07, spdf08))
rm(spdf04,spdf05, spdf06, spdf07,spdf08)

data <- data.table(rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside"])) #sandeel.dt #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside"])
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data$week <- strftime(data$YMD_time, format="%W", tz="UTC")
gps.dt$week <- strftime(gps.dt$YMD_time, format="%W", tz="UTC")
max(data$sA)


l <- c("April", "May", "June", "July", "August")
names(l) <- c("04", "05", "06", "07", "08")

## "AlbjoernLing", "Engelsk_Klondyke", "Vikingbanken", "Nordgyden", "Ostbanken", "Outer_Shoal"
## "Inner_Shoal_East_2016", "Inner_Shoal_North", "Inner_Shoal_test", "Inner_Shoal_West_2018"
## "Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest"

a = c("Vestbanken_North", "VestbankenSouthEast", "VestbankenSouthWest")
spdf_month.dt <- spdf_month[month %in% unique(gps.dt[area%in%a]$month)]

#Engelsk_Klondyke : xlim = c(3.75, 4.6), ylim=c(57.5, 57.85) 
#Innershoal       : xlim = c(3, 4.6), ylim = c(56.55, 57.2)
#Vestbanken       : xlim = c(4.75, 6.7), ylim = c(56.7, 57.5)
#Vinkingbanken    : xlim = c(2.47, 2.83), ylim=c(60.1, 60.7)
#AlbjoernLing     : xlim = c(2.4, 3.4), ylim=c(57.4, 58.2)
#Ostbanken        : xlim = c(3, 4), ylim=c(57.6, 57.9)
#Outer_Shoal      : xlim = c(3.82, 5.1), ylim=c(57.09, 57.53)


#ggplot() +
ggplot(data = w) + geom_sf() + coord_sf(xlim = c(4.75, 6.7), ylim = c(56.7, 57.5), expand = FALSE, datum = NA)+ 
  theme_bw(base_size=15) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size=5), legend.text = element_text(size=12), legend.text.align = 0.5, legend.title = element_text(size=15, hjust = 0.5 ))+ #, strip.text = element_text(size = 8, margin = margin(1,0,1,0, "mm"))
  #scale_y_continuous(breaks = c(57.5, 57.6, 57.7, 57.8)) + #, labels = c("56.8", "57.0", "57.2")
  #scale_x_continuous(breaks = c(3.8, 4, 4.2, 4.4)) + #, labels = c("56.8", "57.0", "57.2")
  #annotation_scale(location = "bl") + #scale bar
  #geom_polygon(data=spdf_month.dt[area%in%a], aes(long, lat, group=area), colour="black", alpha =0.2) + 
  geom_point(data=gps.dt[area%in%a & !area%in%"outside"], aes(x=Longitude, y=Latitude, colour=vessel), size=.05, alpha =0.2) + #theme(legend.position = "none", axis.title = element_blank())+
  #geom_point(data=data[area%in%a], aes(Longitude, Latitude, size = sA), alpha = 0.5)+ 
  #scale_size_continuous(limits = c(0, 439373.7), range = c(0.1,20), breaks = c(500,5000, 10000, 50000), 
  #                      name = expression(atop("",atop(textstyle("Sandeel"), 
  #                                                     # atop(textstyle("target"),
  #                                                     atop(textstyle("school NASC"),
  #                                                          atop(textstyle("("*~ m^2~nmi^-2*')'))))))
  #                      )+
  #scale_color_grey(name="month")+
  #facet_wrap(~month, labeller = labeller(month=l))+ #, scales="free"
  facet_wrap(~week)+
  labs(x="Longitude", y="Latitude")
#
#


data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside"]) #sandeel.dt
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- subset(data, data$area=="VestbankenSouthWest") #"VestbankenSouthWest" / "Engelsk_Klondyke"


#
summary(lm(Latitude~month, data=data))



#==============================================================#
####     NASC change by area (large scale distribution)     ####
#==============================================================#

#== areas more than 1 coverage ==#
data$No <- as.numeric(data$No)
a <- data.table(aggregate(data[!coverage_name%in%"x"]$No, list(data[!coverage_name%in%"x"]$area), FUN=max))
a <- a[x>1]$Group.1

ggplot()+theme_bw()+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90), legend.position = "bottom", panel.spacing = unit(0, "null"))+
  geom_boxplot(data=data[!coverage_name%in%"x" & area%in%a], aes(x=as.factor(format(as.POSIXct(time_mean, 'UTC'), format="%m-%d")), y=log(sA), fill=vessel))+
  facet_grid(.~area, scales="free_x")+
  labs(y="log(school NASC)")
#
ggplot()+theme_bw()+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90), legend.position = "bottom", panel.spacing = unit(0, "null"))+
  geom_boxplot(data=data[!coverage_name%in%"x" & area%in%a], aes(x=as.factor(format(as.POSIXct(time_mean, 'UTC'), format="%m-%d")), y=log(sA), fill=vessel))+
  facet_grid(.~area, scales="free_x")+
  labs(y="log(school NASC)")







#=============================#
#### time series analysis  ####
#=============================#

#== coverage data ==#
data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #sandeel.dt #& school_area >= median(School_EROS.dt$school_area)
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- data[order(vessel, area, YMD_time),]
setDT(data)[, time_diff:=as.numeric(difftime(data$YMD_time, lag(data$YMD_time), units = "hours"))]
setDT(data)[, coverage:=as.numeric(1)]
data$coverage <- as.numeric(data$coverage)

#== run by vessel ==#
v = as.character("SD1032") # "SD1031" / "SD1032" / "EROS"
tmp <- data[vessel %in% v] 

#==============================================================#
for (i in 2:nrow(tmp)){
  if (tmp$time_diff[i] >= 24)                                         #if time_diff is over 24 hours
    tmp$coverage[i] <- tmp$coverage[(i-1)] + 1                        #coverage number + 1
  else if (tmp$area[i] != tmp$area[(i-1)])                            #if time_diff is less than 24h and area[i] is not equal to area[i-1]
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
ggplot() + my_theme() + theme(axis.title = element_blank(), axis.text = element_text(size=5,), axis.ticks.length  = unit(1, "mm"), strip.text = element_text(size = 7))+
  geom_path(data = gps.dt[vessel %in% v], aes(x = Longitude, y = Latitude)) + 
  geom_point(data = tmp, aes(Longitude, Latitude, colour=as.factor(vessel))) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  facet_wrap(~coverage, scales="free")
assign(paste("tmp", v, sep = "_"), tmp)

## combine 3 data.table ## (after making 3 data.frame)
data <- data.table(rbind(tmp_EROS, tmp_SD1031, tmp_SD1032))
setDT(data)[, time_diff:=NULL]
rm(tmp_EROS, tmp_SD1031, tmp_SD1032, tmp)

#== count number of schools  ==#
data[, .(cnt= sum(.N)), by= c("vessel", "coverage", "area")]
gps.dt[, .(cnt= sum(.N)), by= c("vessel", "coverage", "area")]

#== define coverage name ==#
setDT(data)[, coverage_no := paste(vessel, coverage, sep = "-")]
name <- as.data.table(as.table(with(data,by(area,coverage_no,function(xx)names(which.max(table(xx)))))))
setDT(name)[, vessel := gsub('-.*',"",coverage_no)]
setDT(name)[, group_no := order(coverage_no), by  = c("N","vessel")]
setDT(name)[, coverage := ifelse(gsub('.*-',"",name$coverage_no)==("F") | gsub('.*-',"",name$coverage_no)==("x"), gsub('.*-',"",coverage_no), paste(vessel, N, group_no, sep="-"))]
temp <- data.table(coverage_no = name$coverage_no, coverage_name = name$coverage)
data <- merge(x = data, y = temp, by = "coverage_no", all.x = TRUE)
setDT(data)[, sA_sum:= sum(sA), by=c("coverage_name", "vessel")]


#== check if the name correctly assigned ==#
v<- "SD1031"
ggplot() + my_theme() + 
  geom_path(data = gps.dt[!area %in% "outside" & !coverage_name %in% "x" & vessel %in% v ], 
            aes(x = Longitude, y = Latitude), lwd =.1) +
  #geom_polygon(data= spdf, aes(long,lat, group=area), col="black", alpha=.2) + 
  geom_point(data = data[vessel %in% v],
             aes(x = Longitude, y = Latitude), 
             shape = 1, size = 1, colour = "red") + 
  facet_wrap(~coverage_name, scales = "free")
#==============================#
x_gps <- with(gps.dt, aggregate(gps.dt[,c("Latitude", "Longitude", "YMD_time")], list(coverage_name, distance_sum, vessel), mean))
setnames(x_gps, c("Group.1", "Group.2", "Group.3"), c("coverage_name", "distance_sum", "vessel"))
x_dat <- with(data, aggregate(data[,c("Latitude", "Longitude", "YMD_time")], list(coverage_name, sA_sum), mean))
setnames(x_dat, c("Group.1", "Group.2"), c("coverage_name", "sA_sum"))
x <- data.table(merge(x = x_gps, y = x_dat, by = "coverage_name", all.x = TRUE))
x$month.x <- strftime(x$YMD_time.x, "%m", tz="UTC")
ggplot(data=x[!coverage_name%in%c("F","x")]) + theme(legend.position = "none") +
  geom_polygon(data=spdf, aes(long,lat, group=area), col="black", alpha=.2)+
  geom_point(aes(Longitude.x, Latitude.x, colour=coverage_name), shape=1) + 
  geom_point(aes(Longitude.y, Latitude.y, colour=coverage_name), shape=2) + 
  scale_y_continuous(limits = c(56.5,58.5))+
  facet_wrap(month.x~vessel)


#==  calculate sL   ==#
x <- x[!coverage_name%in%c("F","x")]
x$sL <- x$sA_sum/x$distance_sum
setDT(x)[, area:=sub(".*- *(.*?) *-.*", "\\1", x$coverage_name)]
x <- x[order(area, YMD_time.x),]
setDT(x)[, group_no := order(YMD_time.x), by  = c("area")]

cov_name <- x[!is.na(sA_sum) & !area%in%"Nordgyden"]$coverage_name

ggplot() +theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text = element_text(size=5,), axis.ticks.length  = unit(1, "mm"), strip.text = element_text(size = 7))+
  geom_path(data = gps.dt[coverage_name %in% cov_name], 
            aes(Longitude, Latitude, colour = vessel)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + 
  facet_wrap(~coverage_name, scales="free")

ggplot(data = x[!is.na(sA_sum) & 
                  !area%in%"Nordgyden" & 
                  !coverage_name %in% c("SD1031-Inner_Shoal_North-1", "SD1032-Inner_Shoal_East_2016-1")],
       aes(x=YMD_time.x, y=log(sL), colour=vessel))+
  theme_bw() + my_theme() + theme(legend.position = "none", axis.title.x = element_blank())+
  geom_point() +
  geom_text(aes(label = area), hjust = 0.5,  vjust = 0, size = 3) +
  #facet_wrap(~area, scales="free_x")+ 
  labs(y="log(total school NASC / travel distance)")


#==  calculate sA*area by every 1 nmi  ==#
gps.dt <- gps.dt[order(vessel, YMD_time),]
gps.dt[, distance_cum := cumsum(distance), by = vessel][, distance_cum_nmi := distance_cum / 1852]
gps.dt[, distance_group := as.factor(floor(distance_cum_nmi))]
gps.dt[, distance_sum_nmi := sum(distance), by=c("vessel", "distance_group")]

grid.dt <- gps.dt[ , .(YMD_time_min = min(YMD_time),
                       YMD_time_max = max(YMD_time), 
                       Latitudemin = min(Latitude), Latitudemax = max(Latitude), Latitudemean = mean(Latitude), 
                       Longitudemin = min(Longitude), Longitudemax = max(Longitude), Longitudemean = mean(Longitude), 
                       distance_sum_nmi = distance_sum_nmi), 
                   by = .(vessel, distance_group)] 
grid.dt <- unique(grid.dt)

grid.dt[, diff_time := c(0, diff(as.numeric(YMD_time_max), units = "secs")), by = c("vessel")]
grid.dt[, YMD_time_min2 := as.POSIXct(as.numeric(YMD_time_max)-(diff_time-1), origin = "1970-01-01", tz = "UTC"), by = c("vessel")]
grid.dt[, YMD_time_min := c(min(YMD_time_min), YMD_time_min2[-1]), by = c("vessel")][, YMD_time_min2 := NULL][, diff_time := NULL]


area.confirm <- data.table(with(gps.dt, aggregate(gps.dt[,c("area","coverage_name")], 
                                                  list(vessel, distance_group), 
                                                  function(x) names(which.max(table(x))))) )
area.confirm <- area.confirm[order(Group.1, Group.2),]
grid.dt$area.confirm <- area.confirm$area
grid.dt$coverage_name.confirm <- area.confirm$coverage_name

data[, floor_date:= lubridate::floor_date(YMD_time)][grid.dt, on =. (floor_date >= YMD_time_min, floor_date <= YMD_time_max, 
                                                          vessel == vessel), 
                                          distance_group := distance_group][, floor_date := NULL]


data[grid.dt, on =. (distance_group == distance_group, vessel == vessel),
     distance_sum_nmi := distance_sum_nmi]

data[, sA_nmi := sum(sA * school_area) / distance_sum_nmi, by = c("vessel", "distance_group")]

grid.dt[data, on =. (distance_group == distance_group, vessel == vessel),
        c("sA_nmi", "area", "area_2", "coverage_name") 
        := .(sA_nmi, area, area_2, coverage_name)]

grid.dt$sA_nmi[is.na(grid.dt$sA_nmi)] <- as.numeric(0)

grid.dt <- mutate (grid.dt, area_2.confirm = case_when (area.confirm=="AlbjoernLing" ~ "English Klondyke", 
                                                        area.confirm=="VestbankenSouthEast" ~ "Vestbanken",
                                                        area.confirm=="VestbankenSouthWest" ~ "Vestbanken", 
                                                        area.confirm=="Vestbanken_North" ~ "Vestbanken",
                                                        area.confirm=="Vikingbanken" ~ "Vikingbanken", 
                                                        area.confirm=="Engelsk_Klondyke" ~ "English Klondyke",
                                                        area.confirm=="Inner_Shoal_East_2016" ~ "Inner Shoal", 
                                                        area.confirm=="Inner_Shoal_North" ~ "Inner Shoal",
                                                        area.confirm=="Inner_Shoal_West_2018" ~ "Inner Shoal", 
                                                        area.confirm=="Inner_Shoal_test" ~ "Inner Shoal",
                                                        area.confirm=="Nordgyden" ~ "Nordgyden", 
                                                        area.confirm=="Ostbanken" ~ "English Klondyke", 
                                                        area.confirm=="Outer_Shoal" ~ "Vestbanken",
                                                        TRUE ~ "NA"))
grid.dt$area_2 <- ifelse(is.na(grid.dt$area_2), 
                         grid.dt$area_2.confirm, grid.dt$area_2)
grid.dt$area <- ifelse(is.na(grid.dt$area), 
                       grid.dt$area.confirm, grid.dt$area)
grid.dt$coverage_name <- ifelse(is.na(grid.dt$coverage_name), 
                                grid.dt$coverage_name.confirm, grid.dt$coverage_name)
grid.dt[, area_2.confirm:= NULL][, area.confirm := NULL][, coverage_name.confirm := NULL]
rm(area.confirm)
grid.dt <- grid.dt |> mutate_if(is.character, as.factor)

# time of day, week
grid.dt <- grid.dt |>
  mutate(time = as.numeric(strftime(YMD_time_mean, "%H", tz="UTC"))) |>
  mutate(week = as.numeric(strftime(YMD_time_mean, "%W", tz="UTC"))) |>
  mutate(month = strftime(YMD_time_mean, "%m", tz="UTC")) |>
  mutate(time = case_when (time <= 04  ~ "0-4",
                           time <= 08  ~ "4-8",
                           time <= 12  ~ "8-12",
                           time <= 16  ~ "12-16",
                           time <= 20  ~ "16-20",
                           time <= 24  ~ "20-24")) |>
  mutate(week = case_when (week <= 18  ~ "Apr22-May5",
                           week <= 20  ~ "May06-May19",
                           week <= 22  ~ "May20-Jun02",
                           week <= 24  ~ "Jun03-Jun16",
                           week <= 26  ~ "Jun17-Jun30",
                           week <= 28  ~ "Jul01-Jul14",
                           week <= 31  ~ "-Aug04",
                           week <= 33  ~ "-Aug18"))
grid.dt$time = factor(grid.dt$time, levels = c('0-4', '4-8', '8-12',
                                               '12-16', '16-20', '20-24'), 
                      ordered = TRUE)
grid.dt$week = factor(grid.dt$week, levels = c("Apr22-May5", "May06-May19", "May20-Jun02",
                                               "Jun03-Jun16", "Jun17-Jun30","Jul01-Jul14", "-Aug04","-Aug18"), 
                      ordered = TRUE)

# relative time to sunrise-sunset #
library("suncalc")
latlon <- subset(grid.dt, select=c(Latitudemean, Longitudemean, YMD_time_mean))
colnames(latlon) <- c("lat", "lon", "date")
latlon$date <- as.Date(latlon$date, format = '%Y-%m-%d')
sun.df <- getSunlightTimes(data = latlon, keep = c("sunrise", "sunset"))
grid.dt <- cbind(grid.dt, sun.df[, c("sunrise", "sunset")])
grid.dt$day_night <- ifelse(grid.dt$YMD_time_max < grid.dt$sunset & grid.dt$YMD_time_min > grid.dt$sunrise, "day", "night")
grid.dt$hour_from_sunrise <- as.numeric(difftime(grid.dt$YMD_time_mean, grid.dt$sunrise), units="hours")
grid.dt$sunset_sunrise <- as.numeric(difftime(grid.dt$sunset, grid.dt$sunrise), units="hours")
grid.dt$relative_time <- grid.dt$hour_from_sunrise / grid.dt$sunset_sunrise
rm(latlon, sun.df)

grid.dt <- grid.dt |>
  mutate(relative_time_2 = case_when (relative_time <= 0  ~ "before_sunrise",
                                      relative_time <= .25  ~ "0-25",
                                      relative_time <= .5 ~ "25-50",
                                      relative_time <= .75  ~ "50-75",
                                      relative_time <= 1.0  ~ "75-100",
                                      TRUE ~ "after_sunset"))
grid.dt$relative_time_2 = factor(grid.dt$relative_time_2, levels = c('before_sunrise', '0-25', '25-50',
                                                                     '50-75', '75-100', 'after_sunset'), 
                                 ordered = TRUE)







#======================#
####  Ping distance ####
#======================#
library("rjson")
#SD1032
test <- fromJSON(file="S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json")   
#SD1031
test <- fromJSON(file="S2019_SAILDRONE_1031/EXPORT/bottomEchogramPlot_T20190424_10291908-20190820_12575243.json") 
#EROS
test <- fromJSON(file="S2019847_PEROS_3317/EXPORT/bottomEchogramPlot_T20190423_16561743-20190512_19512565.json") 

test <- data.table(PingNumber = test$pingNumber, PingDistance = as.numeric(test$vesselDistance))
setDT(test)[, Diff:=c(0, diff(test$PingDistance))][, distance:= Diff*1852][,Diff:=NULL][,PingDistance:=NULL]
summary(test$distance)
#
#
