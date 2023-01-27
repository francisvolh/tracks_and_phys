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
  filter(species == "PEBO") %>% 
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

#kernels

for_kernel <-depth_gps_data %>% 
  filter(in_dive == 1) %>% 
  mutate(
    colorKernel = case_when(
      species == "PEBO" ~ "cyan",
      species == "GUCO" ~ "brown1"
    )
  )

for (i in unique(for_kernel$species)) {
  
  oneSpecies <- for_kernel %>% 
    filter(species == i)
  
  onespf <- sf::st_as_sf(oneSpecies, 
                         coords = c("lon","lat"),
                         crs = sf::st_crs(4326))
  
  # convert to sp object if needed
  one.sp.points <- as(onespf, "Spatial")
  
  onekernel<-adehabitatHR::kernelUD(one.sp.points)
  
  kernelTest50<-adehabitatHR::getverticeshr(onekernel, 50)
  kernelTest75<-adehabitatHR::getverticeshr(onekernel, 75)
  kernelTest95<-adehabitatHR::getverticeshr(onekernel, 95)
  
  
  
  p <-ggplot() + 
    geom_sf(data= sf::st_as_sf(kernelTest50), aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel)#, aes(fill = id, alpha = 0.5)
    )+
    geom_sf(data= sf::st_as_sf(kernelTest75), aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel) #, aes(fill = id, alpha = 0.5)
    )+
    geom_sf(data= sf::st_as_sf(kernelTest95), aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel)#, aes(fill = id, alpha = 0.5)
    )+
    #geom_sf(data= sf::st_as_sf(one.grizzly.mcp), aes(fill = id, alpha = 0.5)) +
    #scale_fill_discrete(name = "Animal id")+
    #geom_point(data = xy.obs, aes(x=LONGITUDE , y=LATITUDE))+
    geom_sf(data = world) +
    coord_sf(crs = 4326, xlim = range(for_kernel$lon), ylim = range(for_kernel$lat)) +
    #annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
    #        color="black") +
    guides(color = guide_legend(override.aes = list(size = 4)) )+ 
    guides(alpha = "none", color = "none", size = "none")+
    ggtitle(i)+
    theme_bw()+
    theme(legend.position = "none")
  print(p)
  
}


trialspdf <- sf::st_as_sf(for_kernel, 
                       coords = c("lon","lat"),
                       crs = sf::st_crs(4326))
trialspdf <- as(trialspdf, "Spatial")

trialkernel<-adehabitatHR::kernelUD(trialspdf[,8])


image(trialkernel[[2]])

plot(getverticeshr(trialkernel[[1]], 95), add = TRUE)

# creating SpatialPolygonsDataFrame

kd_names <- names(trialkernel)

ud <- lapply(trialkernel, function(x) try(getverticeshr(x, 95)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- kd_names[i]
})
sdf_poly <- Reduce(rbind, ud)
plot(sdf_poly)

ud2 <- lapply(trialkernel, function(x) try(getverticeshr(x, 70)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud2), function(i) {
  row.names(ud2[[i]]) <<- kd_names[i]
})
sdf_poly2 <- Reduce(rbind, ud2)
plot(sdf_poly2)

ud3 <- lapply(trialkernel, function(x) try(getverticeshr(x, 50)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud3), function(i) {
  row.names(ud3[[i]]) <<- kd_names[i]
})
sdf_poly3 <- Reduce(rbind, ud3)
plot(sdf_poly3)


df <- fortify(sdf_poly)
df2 <- fortify(sdf_poly2)
df3 <- fortify(sdf_poly3)

g <- ggplot() +
  geom_polygon(data=df3, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  geom_polygon(data=df2, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  geom_polygon(data=df, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  #coord_equal() +
  geom_sf(data = world) +
  coord_sf(xlim = range(for_kernel$lon), ylim = range(for_kernel$lat)) +
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")
g
g + facet_wrap(~id)