my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#======================#
####   make maps    ####
#======================#
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("ggplot2")
library("rnaturalearthhires")
library("ggspatial") #for scale bar
lapply(c("gps.Rdata",  "Data/spdf.Rdata", "Data/for_map/ctd_EROS.Rdata", 'Data/for_map/EROS.Rdata', 
         'Data/for_map/sd1031.Rdata', 'Data/for_map/sd1032.Rdata'),load,.GlobalEnv)


# zoom-out map #
library("ggthemes") # for theme_map
w <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data = w) + geom_sf() + 
  theme_bw(base_size=15) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  #theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))+
  coord_sf(xlim = c(-5, 9), ylim = c(54,62.3), expand = FALSE, datum = NA)+ #
  annotate(geom = "text", x = 7.6, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 7)+
  annotate(geom = "text", x = -4, y = 57.2, label = "UK", fontface = "bold", color = "grey80", size = 7)+
  geom_polygon(data = spdf, aes(long, lat, group=area), colour="black", fill="red", alpha =0.2) + 
  #geom_point(data=sta.ind, aes(x=Longitude, y=Latitude), size=1, shape=1, col="blue")+
  #geom_point(data=School_EROS_bio.df, aes(x=Longitude, y=Latitude), size=1, shape=1, col="blue", stroke=2)+
  #geom_point(data=sandeel.dt[!area %in% "outside"], aes(x=Longitude, y=Latitude, col=area), size=1, shape=1)+
  #geom_point(data=sandeel.dt[area %in% "outside"], aes(x=Longitude, y=Latitude), size=1, shape=1, col="black")+
  geom_point(data=ctd_EROS.dt, aes(x=Longitude, y=Latitude), size=1, col='red')+
  #geom_path(data=gps.dt, aes(x=Longitude, y=Latitude, colour=vessel), size=.1)+ theme(legend.position = "none", axis.title = element_blank())+
  labs(x="Longitude", y="Latitude")
#
#
p<-ggplot(data = w) + geom_sf() + theme_bw(base_size=15) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))+
  coord_sf(xlim = c(-5, 9), ylim = c(54,62.3), expand = FALSE, datum = NA)+ #
  annotate(geom = "text", x = 7.6, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 7)+
  annotate(geom = "text", x = -4, y = 57.2, label = "UK", fontface = "bold", color = "grey80", size = 7)+
  geom_path(data=sd1031.df, aes(x=Longitude, y=Latitude), size=.1, colour="blue")+ theme(legend.position = "none", axis.title = element_blank())+
  geom_path(data=sd1032.df, aes(x=Longitude, y=Latitude), size=.1, colour="green")+
  #geom_path(data=subset(EROS.df, area ==  "Engelsk_Klondyke"), aes(x=Longitude, y=Latitude), size=.1, colour="red") +
  theme(axis.title = element_blank())#+labs(x="Longitude", y="Latitude")
#
#ggsave(p, filename = "map2SD.png",  bg = "transparent")
#


library(ggrepel)
spdf_mean <- data.table(aggregate(cbind(long, lat) ~ area, data = spdf, FUN = 'mean'))
ggplot(data = w) + geom_sf() + 
  #theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))+
  coord_sf(xlim = c(-5, 9), ylim = c(56,62.3), expand = FALSE, datum = NA)+ #
  annotate(geom = "text", x = 7.6, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 7)+
  annotate(geom = "text", x = -4, y = 57.2, label = "UK", fontface = "bold", color = "grey80", size = 7)+
  geom_polygon(data = spdf, aes(long, lat, group=area), colour="black", fill="transparent", alpha =0.2) + 
  
  geom_path(data = gps.dt[vessel %in% "EROS"], aes(x=Longitude, y=Latitude), size=.1, colour="#619CFF", alpha = .3) + annotate(geom = "text", x = 5.9, y = 57.8, label = "EROS", color = "#619CFF", size = 3, alpha = .7) +
  geom_path(data = gps.dt[vessel %in% "SD1031"], aes(x=Longitude, y=Latitude), size=.1, colour="#F8766D", alpha = .3) + annotate(geom = "text", x = 2.6, y = 61.5, label = "SD1031", color = "#F8766D", size = 3, alpha = .7) +
  geom_path(data = gps.dt[vessel %in% "SD1032"], aes(x=Longitude, y=Latitude), size=.1, colour="#00BA38", alpha = .3) + annotate(geom = "text", x = 2, y = 58.9, label = "SD1032", color = "#00BA38", size = 3, alpha = .7) +

  geom_text_repel(data = spdf_mean[area %in% c("Inner_Shoal_North", "Inner_Shoal_West_2018", "Inner_Shoal_test", "Inner_Shoal_East_2016", "AlbjoernLing")],
                   aes(x = long, y = lat, label = area),
                   box.padding   = .5, label.padding = .15, point.padding = .1, 
                   force = 40, max.overlaps = Inf, 
                   min.segment.length = 0 ,size = 3,
                  xlim  = c(NA, 1)) +
  geom_text_repel(data = spdf_mean[area %in% c("Ostbanken", "Engelsk_Klondyke")],
                  aes(x = long, y = lat, label = area),
                  box.padding   = .5, label.padding = .15, point.padding = .1, 
                  force = 40, max.overlaps = Inf, 
                  min.segment.length = 0 ,size = 3,
                  xlim  = c(4.5, 9), ylim = c(58.5, 57.8)) +
  geom_text_repel(data = spdf_mean[area %in% c("Outer_Shoal", "Vestbanken_North")],
                  aes(x = long, y = lat, label = area),
                  box.padding   = .5, label.padding = .15, point.padding = .1, 
                  force = 40, max.overlaps = Inf, 
                  min.segment.length = 0 ,size = 3,
                  xlim  = c(6.5, 9), ylim = c(56.3, 57.8)) +
  geom_text_repel(data = spdf_mean[area %in% c("VestbankenSouthEast",  "VestbankenSouthWest")],
                  aes(x = long, y = lat, label = area),
                  box.padding   = .5, label.padding = .15, point.padding = .1, 
                  force = 40, max.overlaps = Inf, 
                  min.segment.length = 0 ,size = 3,
                  ylim  = c(56, 56.5), xlim = c(-1,NA)) +
  geom_text_repel(data = spdf_mean[area %in% c("Vikingbanken", "Nordgyden")],
                  aes(x = long, y = lat, label = area),
                  box.padding   = .5, label.padding = .15, point.padding = .1, 
                  force = 40, max.overlaps = Inf, 
                  min.segment.length = 0 ,size = 3,
                  xlim  = c(NA, 2.2)) +
  my_theme() + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
  
  
  
  
  