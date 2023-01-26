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
  group_by(species) %>% #or per dep ID?
  filter(in_dive == 1) %>% 
  summarise(
    MaxDepth = max(depth),
    MinDepth = min(depth), 
    meanDepth = mean(depth),
    sdDepth = sd(depth),
    .groups = "drop" 
  )

#need to add the base map! or a terrain basemap
depth_gps_data %>% 
  filter(in_dive == 1) %>%
  filter(species == "PEBO") %>% 
  ggplot()+
  geom_point(
    aes(x= lon, y=lat, colour = as.factor(in_dive)), cex = 0.01
  )

