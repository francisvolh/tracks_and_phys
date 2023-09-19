#library(ggspatial)
library(ggplot2)
library(cowplot)
library(seabiRd)#colour paletes

####Create object for basemap
#Bring ESRI world shapefile into R
#setwd("C:/Users/franc/OneDrive - McGill University/Documents/Research/2021/Categorizacion 2020/chuita/gadm36_PER_shp")
world <- sf::st_read("data/gadm36_PER_shp/gadm36_PER_0.shp")

#Use geom_sf to add basemap to your plot

####Plot points on graph
theme_set(theme_light())

#create Date Only Label for ALL INFO
all_info$dateOnly<-as.Date(all_info$date_time)

#get day with most Trips  
trips$dateOnly<-as.Date(trips$start_time)

unique(trips$dateOnly)
table(trips$dateOnly)

##MAP with day with MOST trips
toplot<-all_info[which(all_info$dateOnly=="2019-11-18"),]

#df to plot, one bird, one trip, etc
toplot<-all_info[which(all_info$trip_id=="7_G24PEBO2019"),]
##BIRD THAT SLEPT IN OTHER ISLAND 6_G24PEBO2019
## 7_G24PEBO2019 
## 2_G04PEBO2019
## 10_G04PEBO2019


nrow(toplot)
unique(toplot$Bird2)
unique(toplot$trip_id)

# 5, 8, 11
toplot<-gps.data[which(gps.data$dep_id=="C14PEBO_20191121_A161_S1" & gps.data$trip_id == 4),]
nrow(toplot)

plot <- ggplot() +
  geom_point(data = toplot, aes(x = lon, y = lat), cex= 3) +
  xlab(NULL)+
  ylab("Latitude")+
  geom_sf(data = world, aes()) +#+ #add basemap
  coord_sf(crs = 4326, xlim = c(-78.99, -78.94), ylim = c(-8.59, -8.53)) #+xlim = range(toplot$lon), ylim = range(toplot$lat)
  #annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
   #        color="black") +
 # guides(color = guide_legend(override.aes = list(size = 4)) ) +
  #theme(
   # legend.position = c(0.99, 0.99),
    #legend.justification = c(1,1),
    #legend.text = element_text(size = 15)
  
plot


#TWO big plots for year 2018 and 2019
######

toplot<-gps.data[which(gps.data$year=="2018"),]

plot1 <- ggplot() +
  geom_path(data = toplot, aes(x = lon, y = lat, color = dep_id ), cex= 1) +
  xlab(NULL)+
  ylab("Latitude")+
  geom_sf(data = world, aes()) +#+ #add basemap
  coord_sf(crs = 4326, xlim = c(-79.5, -78.4), ylim = c(-9.0, -8)) +
  annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
           color="black") +
  guides(color = guide_legend(override.aes = list(size = 4)) ) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1,1),
    legend.text = element_text(size = 15)
  )

toplot<-all_info[which(all_info$Year=="2019"),]

plot2 <- ggplot() +
  geom_point(data = toplot, aes(x = Long, y = Lat, color = Species ), cex= 1, show.legend = FALSE) +
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(data = world, aes()) +#+ #add basemap
  coord_sf(crs = 4326, xlim = c(-79.5, -78.4), ylim = c(-9.0, -8)) +
  annotate(geom="text", x=-78.6, y=-8.4, label="2019", size= 13,
           color="black") +
  guides(col = F)
pcol<-plot_grid(plot1, plot2, labels = c('A', 'B'), ncol = 1)
pcol


dev.copy(jpeg, filename = "mapsYears.jpg", width = 4500, height = 3000, 
         res = 500)
dev.off()

#to make loop of all plots### SEABIRDS DOES This now


birdiedat<-subset(all_info, !is.na(all_info$trip_id))

birdiedat<-droplevels(birdiedat)

birdies<-unique(birdiedat$trip_id)
#birdies<-c("1_A01PEBO2019","2_A01PEBO2019","3_A01PEBO2019","4_A01PEBO2019")

##LOOPING ALL TRIPS TO CHEC WHICH ARE GOOD OR NOT
for (i in birdies) {
  toplot<-birdiedat[which(birdiedat$trip_id==i),]
  p <- ggplot() +
    geom_point(data = toplot, aes(x = Long, y = Lat, color = "red" ), cex= 1) +
    xlab("Longitude")+
    ylab("Latitude")+
    geom_sf(data = world, aes()) +#+ #add basemap
    coord_sf(crs = 4326, xlim = c(-80, -78.4), ylim = c(-9.2, -8)) +
    theme(legend.position = "none")+
    ggtitle(paste(i))

  print(p)
  readline('next')
  
}

###
## one plot
##
b<-b[!is.na(b$lon),]
plot1 <- ggplot() +
  geom_point(data = aaa, aes(x = lon, y = lat, color = "red" ), cex= 1) +
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(data = world, aes()) +#+ #add basemap
  coord_sf(crs = 4326, xlim = range(merged.GPS$lon), ylim = range(merged.GPS$lat))# +
  #annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
   #        color="black") +
  guides(color = guide_legend(override.aes = list(size = 4)) )
  
  
  
#############################################
#############################################
#############################################
##plot fast trips
fastTracks <- gps.data %>% 
  filter(!is.na(trip_id)) %>% 
  filter(unique_trip %in% unique(fasts$unique_trip))
nrow(fastTracks)

fastTracks <- fastTracks %>%
  mutate(
 SpeedCol = case_when(speed >= 100 ~ "red", 
                     speed < 100 ~ "blue")
  )

#birdies<-c("1_A01PEBO2019","2_A01PEBO2019","3_A01PEBO2019","4_A01PEBO2019")

##LOOPING ALL FAST TRIPS 
birdies<-unique(fastTracks$unique_trip)
for (i in birdies) {
  toplot<-fastTracks[which(fastTracks$unique_trip==i),]
  p <- ggplot() +
    geom_point(data = toplot, aes(x = lon, y = lat , color = SpeedCol), cex= 1) +
    scale_color_manual(values = c("red" = "red", "blue" = "blue"))+
    xlab("Longitude")+
    ylab("Latitude")+
    geom_sf(data = world, aes()) +#+ #add basemap
    coord_sf(crs = 4326, xlim = c(-80, -78.4), ylim = c(-9.2, -8)) +
    theme(legend.position = "none")+
    ggtitle(paste(i))
    
    print(p)
    readline('next')
}

#loopoing trip with bouts more than 0.75 hour, RST outputs
for (i in unique(RRR$unique_trip)) {
  toplot<-merged.GPS[which(merged.GPS$unique_trip==i),]
  p <- ggplot() +
    geom_sf(data = world, aes()) +#+ #add basemap
    geom_point(data = toplot, aes(x = lon, y = lat , color = as.factor(activity)), cex= 1) +
    scale_color_manual( values = c("travel" = "black", "foraging" = "blue", "resting" = "red"))+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(crs = 4326, xlim = range(toplot$lon), ylim = range(toplot$lat)) + #xlim = c(-79, -78.8), ylim = c(-8.6, -8.5)
    theme(legend.position = "none")+
    ggtitle(paste(i))
  
  print(p)
  readline('next')
}


###########TO ONLY PLOT 1 trip#############################################
i <-  "G01PEBO_31_20181125-152024 _trip_ 5" # if wnt to do one specicif run inside the loop only

toplot<-gps.data[which(gps.data$unique_trip==i),]

p <- ggplot() +
  geom_sf(data = world, aes()) +#+ #add basemap
  geom_point(data = toplot, aes(x = lon, y = lat, colour = speed), cex= 1) +
  #scale_color_manual(values = c("travel" = "black", "foraging" = "blue", "resting" = "red"))+
  scale_color_gradient(low="yellow", high = "red")+
  xlab("Longitude")+
  ylab("Latitude")+
  
  coord_sf(crs = 4326, xlim = range(toplot$lon), ylim = range(toplot$lat)) + #  xlim = range(toplot$lon), ylim = range(toplot$lat)
  theme(legend.position = "none")+
  ggtitle(paste(i))
p

View(toplot)
#############################################


#sinuosity plots

#get the vectors from sinuosity code
# unique_tripsIDS
#sins_trips$V1

for (i in unique_tripsIDS) {
  
  toplot<-gps.data[which(gps.data$unique_trip==i),]
  
  sinuosity <- sins_trips$V1[which(sins_trips$unique_trips == i)]
  p <- ggplot() +
    geom_sf(data = world, aes()) +#+ #add basemap
    geom_path(data = toplot, aes(x = lon, y = lat), cex= 1) +
    # scale_color_manual( values = c("travel" = "black", "foraging" = "blue", "resting" = "red"))+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(crs = 4326, xlim = range(toplot$lon), ylim = range(toplot$lat)) + #xlim = c(-79, -78.8), ylim = c(-8.6, -8.5)
    theme(legend.position = "none")+
    ggtitle(paste(i,"_",sinuosity))
  
  print(p)
  readline('next')
}




test<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

test<- readRDS(file="C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")
SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")
test<-test %>% 
  filter(!is.na(lat))

test$speedCOL<-"fast"
test[which(test$speed < 90),]$speedCOL<-"flight"
test[which(test$speed < 10),]$speedCOL<-"slow"



hist(test[which(test$dep_id == "G13GUCO_COR08_2019"),]$speed)
hist(test[which(test$dep_id == "G13GUCO_COR08_2019" & test$coldist >5),]$speed)
View(test[which(test$dep_id == "G13GUCO_COR08_2019" & test$speedCOL == "fast"),]$speed)

View(test[which(test$dep_id == "G13GUCO_COR08_2019" & test$speedCOL == "fast"),])

test<-test[which(test$dep_id == "G13GUCO_COR08_2019"),]

#plot all tracks with some reference of speed (seabiRds data cleaned at 150km/h)

for (i in unique(test$dep_id)) {
  subsampi<-test[which(test$dep_id == i),]
  plot1<-ggplot(subsampi, aes(time, coldist, color = speedCOL)) +
    #geom_line() + 
    geom_point()+
    ggtitle(paste(i))
  print(plot1)
  readline('next')
}



##plot only a certain group of trips

tripis<-unique(SUMMATRIPS[which(SUMMATRIPS$active == 0),]$unique_trip)#trips that have no FORAGING classification

for (i in uniquesss) {
  
  toplot<-gps.data[which(gps.data$unique_trip==i),]
  
  #sinuosity <- sins_trips$V1[which(sins_trips$unique_trips == i)]
  p <- ggplot() +
    geom_sf(data = world, aes()) +#+ #add basemap
    geom_point(data = toplot, aes(x = lon, y = lat, colour = speed), cex= 1) +
    # scale_color_manual( values = c("travel" = "black", "foraging" = "blue", "resting" = "red"))+
    xlab("Longitude")+
    ylab("Latitude")+
    scale_color_gradient(low="yellow", high = "red")+
    coord_sf(crs = 4326, xlim = range(toplot$lon), ylim = range(toplot$lat)) + #xlim = c(-79, -78.8), ylim = c(-8.6, -8.5)
    theme(legend.position = "none")+
    ggtitle(paste(i))
  
  print(p)
  readline('next')
}


### MAP FOR THESIS LIT REVIEW

continent <- sf::st_read("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/south america shape/vc965bq8111.shp")

site.loc <-  data.frame(
  lat = c(-8.533850),
  lon = c(-78.963038),
  lab = c("Guañape Norte")
)

#conti.shelf<- sf::st_read("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/Global Margin shape/ContinentalMargins.shp")

peru <-  ggplot()+
  geom_sf(data =  continent)+
  #geom_sf(data =  conti.shelf, fill = "lightblue")+
  geom_point(data = site.loc, aes(x=lon, y=lat), shape = 23, fill = "yellow")+
  geom_text(aes(x=lon, y=lat, label=lab), data=site.loc, size=3,
            nudge_x = 2,
            nudge_y = .5, col = "black")+
  coord_sf(xlim = c(-86, -68), ylim = c(-0, -21))+
  #scale_x_continuous(breaks=c(-70,-72.5,-75,-77.5,-80))+
  geom_curve(
    aes(x = -75, y = -20, xend = -85, yend = -7),
    arrow = arrow(
      length = unit(0.5, "cm")#, 
      #type="both" # Describes arrow head (open or closed)
    ),
    colour = "blue",
    linewidth = 2,
    #angle = 90,
    curvature = 0.25# Anything other than 90 or 0 can look unusual
  )+
    xlab(NULL)+
    ylab(NULL)+
    annotate("text", x = -81.5, y = -13.5, 
             label = "Humboldt Current", colour = "blue", 
             angle = '-45', size = 5)+
    annotate('text', x = -75, y = -10, label = "PERU", size = 5) +
  theme(axis.text =element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = "transparent"), # major grid lines
        panel.grid.minor = element_line(colour = "transparent"),
        #panel.background=element_blank()# minor grid lines
        plot.margin = margin(0, 0, 0, 0, "cm")
  )

rect <- data.frame(
  x = c(-86, -68, -68, -86),
  y = c( 0, 0, -21, -21)
)



sout.ame <- ggplot()+
  geom_sf(data =  continent)+
  coord_sf(xlim = c(-86, -22))+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_polygon(data = rect, aes(x, y, group = 1), colour = "black", fill = "transparent")



sa.coast <- ggdraw(sout.ame) +
    draw_plot(
      {
        peru 
        #theme(panel.border=element_blank())
      },
      # The distance along a (0,1) x-axis to draw the left edge of the plot
      x = 0.35, 
      # The distance along a (0,1) y-axis to draw the bottom edge of the plot
      y = 0.1,
      # The width and height of the plot expressed as proportion of the entire ggdraw object
      width = 0.6, 
      height = 0.6)
  
sa.coast

ggsave( "sa.coast.theor.png", sa.coast, units = 'cm', width = 28, height = 20)
