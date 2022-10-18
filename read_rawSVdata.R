library(data.table)


read.rawSVdata <- function (dir, vessel, category) {
  
  SvSchool.dt <- data.table()
  pattern <- "SvSchool\\s*(.*?)\\s*_T2019"
  files <- list.files(dir, pattern='txt$', full.names=TRUE)
  
  #== read data file(i) ==#
  for(i in 1:length(files)) {
    temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))  ## read text data
    temp <- cbind(category = category, temp)   ## add column category
    temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
    temp <- data.table::melt(temp, id.vars = c(1:13))
    SvSchool.dt <- rbind(SvSchool.dt, temp, fill=TRUE)
    temp <- data.table()
  }
  #=======================#
  
  #== add vessel column ==#
  SvSchool.dt$vessel <- as.character(vessel)
  
  
  setnames(SvSchool.dt, c("variable", "value"), c("SampleNo","Sv"))
  
  return(SvSchool.dt)
  
}




#-------------------------------------------------------------------------------#
#directory                                                 vessel     category  #
#-------------------------------------------------------------------------------#
#'C:/Data/S2019847_PEROS_3317/EXPORT/Sv_SAND_2.11.0-rc1'   EROS        SAND     #
#'C:/Data/S2019847_PEROS_3317/EXPORT/Sv_PSAND_2.11.0-rc1'  EROS        PSAND    #
#'C:/Data/S2019847_PEROS_3317/EXPORT/Sv_OTHER_2.11.0-rc1'  EROS        OTHER    #
#'C:/Data/S2019_SAILDRONE_1032/EXPORT/PSAND'               SD1032      KORONA   #
#'C:/Data/S2019_SAILDRONE_1032/EXPORT/SAND'                SD1032      manual   #
#'C:/Data/S2019_SAILDRONE_1032/EXPORT/OTHER(bubble,noise)' SD1032      noise    #
#'C:/Data/S2019_SAILDRONE_1031/EXPORT/PSAND'               SD1031      KORONA   #
#'C:/Data/S2019_SAILDRONE_1031/EXPORT/SAND'                SD1031      manual   #
#'C:/Data/S2019_SAILDRONE_1031/EXPORT/OTHER'               SD1031      noise    #
#-------------------------------------------------------------------------------#


dir <- choose.dir()
dir <- gsub('\\\\', '\\/', dir)

vessel <- "SD1032" #EROS / SD1031 / SD1032

category <- "noise" #KORONA / manual / noise #SAND / PSAND / OTHER / 



SvSchool.dt <- read.rawSVdata(dir = dir, vessel = vessel, category = category)

assign(paste("Svschool", vessel, category, sep = "_"), SvSchool.dt)






SvSchool_EROS.dt <- rbind(Svschool_EROS_SAND, Svschool_EROS_PSAND, Svschool_EROS_OTHER)
SvSchool_EROS.dt <- SvSchool_EROS.dt[!is.na(Sv)]
save(SvSchool_EROS.dt, file = "C:/Data/rawSv_data/SvSchool_EROS.Rdata")
rm(Svschool_EROS_SAND, Svschool_EROS_PSAND, Svschool_EROS_OTHER)

SvSchool_SD1031.dt <- rbind(Svschool_SD1031_KORONA, Svschool_SD1031_manual, Svschool_SD1031_noise)
SvSchool_SD1031.dt <- SvSchool_SD1031.dt[!is.na(Sv)]
save(SvSchool_SD1031.dt, file = "C:/Data/rawSv_data/SvSchool_SD1031.Rdata")
rm(Svschool_SD1031_KORONA, Svschool_SD1031_manual, Svschool_SD1031_noise)

SvSchool_SD1032.dt <- rbind(Svschool_SD1032_KORONA, Svschool_SD1032_manual, Svschool_SD1032_noise)
SvSchool_SD1032.dt <- SvSchool_SD1032.dt[!is.na(Sv)]
save(SvSchool_SD1032.dt, file = "C:/Data/rawSv_data/SvSchool_SD1032.Rdata")
rm(Svschool_SD1032_KORONA, Svschool_SD1032_manual, Svschool_SD1032_noise)






