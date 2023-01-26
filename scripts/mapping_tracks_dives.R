library(tidyverse)

#diving depths and maps

divingdepths<-readRDS(file.choose())

unique(divingdepths$dep_id)

head(divingdepths)

#this one includes the sitting rock bird C02
depth_gps_data <-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/merged.tdr.gps.RDS")

nrow(depth_gps_data %>% 
  filter(in_dive == 1) )

depth_gps_data %>% 
  group_by(dep_id) %>% #or per dep ID?
  filter(in_dive == 1) %>% 
  summarise(
    MaxDepth = max(depth),
    MinDepth = min(depth), 
    meanDepth = mean(depth),
    sdDepth = sd(depth),
    .groups = "drop" 
  ) %>% 
  View()

#need to add the base map! or a terrain basemap

world <- sf::st_read("data/gadm36_PER_shp/gadm36_PER_0.shp")

depth_gps_data %>% 
  filter(in_dive == 1) %>%
  #filter(species == "PEBO") %>% 
  ggplot()+
  geom_sf(data = world, aes()) +  ##add basemap
  coord_sf(crs = 4326, xlim = range(depth_gps_data$lon), ylim = range(depth_gps_data$lat))+# +
  geom_point(
    aes(x= lon, y=lat, colour = as.factor(species)), cex = 0.01
  )+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme( legend.position = "none")

#maybe plot the change in dive locations with a change in day, every two days or so

# refine the depth? more than 0.7? or check the histograms for shallow dives?


