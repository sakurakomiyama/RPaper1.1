rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)


#================================#
####         Load data        ####
#================================#
#== choose which LDA result will be used for subsequent analysis ==#
dir <- choose.dir()
dir <- gsub('\\\\', '\\/', dir)

lapply(c("gps.Rdata",  paste0(dir,"/sandeel.Rdata")),load,.GlobalEnv)


#============================#
####    Density change    ####
#============================#

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

