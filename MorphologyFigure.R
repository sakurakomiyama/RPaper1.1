library(dplyr)
library(data.table)
library(ggplot2)
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#================================#
####      load data           #### 
#================================#
load("C:/Users/a37907/Desktop/bottom.Rdata")


v <- 1032 #1031 / 1032
scrutiny <- "SAND" # "SAND" (manual scrutiny) / "PSAND" (KORONA scrutiny) 
id <- 9583
file1 <- paste0("SvSchool", id, "_")
dir <- paste0("C:/Users/a37907/Desktop/KnowSandeel15781/Data/S2019_SAILDRONE_", v, "/EXPORT/", scrutiny)
files <- list.files(dir)
file2 <- files[grep(file1, files)]

ID <- as.data.table(read.table(file = paste0(dir, "/", file2), header = TRUE, sep = ","))
rm(file1, dir, files, file2)

#== melt + category, id column ==#
ID <- melt(ID, id.vars = c(1:11))
ID$category <- ifelse(scrutiny == "SAND", "manual", "KORONA") ## add column "category"
ID$id <- id

#==         ==#
ID <- ID[!is.na(ID$value), ]
names(ID)[names(ID) == 'variable'] <- 'SampleNo'
names(ID)[names(ID) == 'value'] <- 'Sv'
ID$Sample.number <- as.numeric(gsub("Sv", "", ID$SampleNo))
#ID <- subset(ID, Frequency==200)

#==  bottom depth  ==#
bottom <- bottom.dt[vessel %like% v]
ID <- merge(x = ID, y = bottom, by = c("PingNumber"), all.x = TRUE)

#== depth_bin of pixel ==#
ID$depth_bin <- (ID$DepthStop- ID$DepthStart)/ID$SampleCount
ID$depthStart <- ID$DepthStart + (ID$depth_bin * (as.numeric(gsub("Sv", "", ID$SampleNo))-1))
ID$depthStop <- ID$depthStart + ID$depth_bin

#== School size(area), School length ==#
setDT(ID)[, pixel_size := distance*depth_bin]
setDT(ID)[, school_area := sum(pixel_size), by = c("id", "Frequency")]
ID$distance_cum[1] <- ID$distance[1]
#for(i in 2:nrow(ID)) {
#  ID$distance_cum[1] <- ID$distance[1]
#  if (ID$PingNumber[i] == ID$PingNumber[i-1]) 
#    ID$distance_cum[i] <- ID$distance_cum[i-1]
#  else
#    ID$distance_cum[i] <- ID$distance_cum[i-1] + ID$distance[i]
#}
setDT(ID)[, school_length := distance/.N, by = c("id", "Frequency","PingNumber")][, school_length := sum(school_length), by = c("id", "Frequency")]

#== mean depth ==#
ID$sV <- 10^(ID$Sv/10)
setDT(ID)[, sV_nmi := sV*4*pi*(1852)^2][, sV_nmi := sV_nmi * depth_bin]
setDT(ID)[, sV_nmi_sample := mean(sV_nmi), by=c("id", "Frequency", "SampleNo")]
setDT(ID)[, meanDepth := mean(c(DepthStart, DepthStop)), by=c("id", "Frequency")]
setDT(ID)[, weighted_meanDepth := weighted.mean((depthStart+(depth_bin/2)),sV_nmi_sample), by=c("id", "Frequency", "PingNumber")]

#== depth from bottom ==#
setDT(ID)[, nor_Depth := weighted_meanDepth/BottomDepth][, DepthfromBottom := BottomDepth-DepthStop]
setDT(ID)[, nor_DepthStart := DepthStart/BottomDepth][, nor_DepthStop := ifelse(DepthStop/max(BottomDepth)>1 , 1 , DepthStop/max(BottomDepth)), by=c("id","Frequency")]


#======================================#
####          make figure           #### 
#======================================#

x_range <- (max(ID$PingNumber) - min(ID$PingNumber))
y_range <- max(ID$BottomDepth)

ggplot(ID[Frequency %in% 200], 
       aes(x = PingNumber, y = depthStart)) + 
  my_theme() + theme(axis.text.x = element_blank(), axis.ticks.x = ) + #legend.key = element_blank(),
  scale_x_continuous(breaks = seq(min(ID$PingNumber), max(ID$PingNumber), by = 1)) +
  scale_y_reverse(limits = c(max(ID$BottomDepth), ID$DepthStart - (y_range * .2))) + 
  geom_tile(colour = "black", lwd=2) + 
  geom_tile(aes(fill = Sv)) +
  scale_fill_gradient(low = "grey95", high = "grey40", 
                      name=expression(atop("",atop(textstyle("S"["V"]),
                                                   atop(textstyle("("*~dB~re~1~m^-1*')')))))) + 
  #scale_fill_gradientn(name=expression(atop("",atop(textstyle("Sv"),
  #                                                  atop(textstyle("("*~dB-re~1~m^-1*')'))))) )+
  geom_line(aes(x = PingNumber, y = BottomDepth)) + 
  labs(x = "Ping number", y = "Depth (m)") +
  #== distance from bottom arrow ==#
  geom_segment(aes(x = PingNumber[max.col(t(depthStart))], 
                   y = min(BottomDepth), 
                   xend = PingNumber[max.col(t(depthStart))], 
                   yend = max(depthStop)), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  annotate("text", x = (ID$PingNumber[max.col(t(ID$depthStart))]), 
           y = min(ID$BottomDepth) + ((max(ID$depthStop) - min(ID$BottomDepth))/2), 
           label= "Distance from the bottom") + 
  #== school height arrow ==# 
  geom_segment(aes(x = min(PingNumber)-(x_range * .25), y = DepthStart, 
                   xend = min(PingNumber)-(x_range * .25), yend = max(ID$depthStop)), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  # dashed line upper end
  geom_segment(aes(x = min(PingNumber)-(x_range * .35), y = min(depthStart), 
                   xend = ID[which.min(ID$depthStart),]$PingNumber, 
                   yend = min(depthStart)), 
               linetype = "dashed") +
  # dashed line lower end
  geom_segment(aes(x = min(PingNumber)-(x_range * .35), y = max(depthStop), 
                   xend = ID[which.max(ID$depthStart),]$PingNumber, 
                   yend = max(depthStop)), 
               linetype = "dashed") +
  annotate("text", x = min(ID$PingNumber)-(x_range * .35), 
           y = max(ID$depthStop) - ((max(ID$depthStop) - min(ID$depthStart)) * 0.5), 
           label= "School height", angle = 90) + 
  #== school length arrow ==#
  geom_segment(aes(x = min(PingNumber) - 0.5, y = DepthStart - (y_range * .1), 
                   xend = max(PingNumber) + 0.5, yend = DepthStart - (y_range * .1)), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  # dashed line start
  geom_segment(aes(x = min(PingNumber) - 0.5, 
                   y = ID[which.min(ID$PingNumber),]$depthStart, 
                   xend = min(PingNumber) - 0.5, yend = DepthStart- (y_range * .15)), 
               linetype = "dashed") +
  # dashed line end
  geom_segment(aes(x = max(PingNumber) + 0.5, 
                   y = ID[which.max(ID$PingNumber),]$depthStart, 
                   xend = max(PingNumber) + 0.5, yend = DepthStart-(y_range * .15)), 
               linetype = "dashed") +
  annotate("text", x = min(ID$PingNumber) + (max(ID$PingNumber) - min(ID$PingNumber)) * 0.5, 
           y = unique(ID$DepthStart) - (y_range * .15), 
           label= "School length") + 
  #== Weighted men depth ==#
  geom_point(aes(y = mean(weighted_meanDepth), 
                 x = (max(PingNumber) + min(PingNumber))/2),
             shape = 21, stroke = 2, col = "black", fill = "transparent") +
  annotate("text", x = min(ID$PingNumber) + (max(ID$PingNumber) - min(ID$PingNumber)) * 0.5, 
           y = mean(ID$weighted_meanDepth) + (y_range * .05), 
           label= "weighted\nmean depth") + 
  #== Area + Perimeter ==#
  annotate("text", x = min(ID$PingNumber) + (max(ID$PingNumber) - min(ID$PingNumber)) * 0.5, 
           y = min(ID$depthStart) + (y_range * .15), 
           label= "Area", size = 5) +
  annotate(geom = "curve", x = min(ID$PingNumber) - 0.5, 
           y = max(ID$BottomDepth) - (y_range*0.1), 
           xend = ID$PingNumber[max.col(t(ID$depthStart))] - round((mean(ID$PingNumber) - min(ID$PingNumber)) * 0.5), 
           yend = max(ID$depthStop[ID$PingNumber == ID$PingNumber[max.col(t(ID$depthStart))] - round((mean(ID$PingNumber) - min(ID$PingNumber)) * 0.5)]), 
    curvature = .3, arrow = arrow(length = unit(0.25, "cm"))) +
  annotate(geom = "text", 
           x = min(ID$PingNumber) - 0.5, 
           y = max(ID$BottomDepth) - (y_range*0.1), 
           label = "Perimeter", hjust = "right")
