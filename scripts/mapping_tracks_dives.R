library(adehabitatHR)
library(ggplot2)
library(dplyr)

##need to update code to SF or terra compatible
# maybe switch to ctmm

#diving depths and maps

divingdepths<-readRDS(file.choose())

unique(divingdepths$dep_id)

head(divingdepths)

#this one includes the sitting rock bird C02
depth_gps_data <-readRDS("E:/05BACKUP July 10 2024/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/merged.tdr.gps.RDS")

nrow(depth_gps_data |> 
  dplyr::filter(in_dive == 1) )

depth_gps_data |> 
  dplyr::group_by(dep_id)|>  #or per dep ID?
  dplyr::filter(in_dive == 1) |> 
  dplyr::summarise(
    MaxDepth = max(depth),
    MinDepth = min(depth), 
    meanDepth = mean(depth),
    sdDepth = sd(depth),
    .groups = "drop" 
  ) |> 
  View()

#need to add the base map! or a terrain basemap

world <- sf::st_read("data/gadm36_PER_shp/gadm36_PER_0.shp")

depth_gps_data |>  
  dplyr::filter(in_dive == 1) |> 
  dplyr::filter(species == "PEBO") |>  
  ggplot2::ggplot()+
  ggplot2::geom_sf(data = world, ggplot2::aes()) +  ##add basemap
  ggplot2::coord_sf(crs = 4326, xlim = range(depth_gps_data$lon), ylim = range(depth_gps_data$lat))+# +
  ggplot2::geom_point(
    ggplot2::aes(x= lon, y=lat, colour = as.factor(species)), cex = 0.01
  )+
  ggplot2::theme_bw()+
  ggplot2::xlab("Longitude")+
  ggplot2::ylab("Latitude")+
  ggplot2::theme( legend.position = "none")

#maybe plot the change in dive locations with a change in day, every two days or so

# refine the depth? more than 0.7? or check the histograms for shallow dives?

#kernels

for_kernel <- depth_gps_data |>  
  dplyr::filter(in_dive == 1) |>  
  dplyr::mutate(
    colorKernel = dplyr::case_when(
      species == "PEBO" ~ "cyan",
      species == "GUCO" ~ "brown1"
    )
  )

for (i in unique(for_kernel$species)) {
  
  oneSpecies <- for_kernel |>  
    dplyr::filter(species == i)
  
  onespf <- sf::st_as_sf(oneSpecies, 
                         coords = c("lon","lat"),
                         crs = sf::st_crs(4326))
  
  # convert to sp object if needed
  one.sp.points <- as(onespf, "Spatial")
  
  onekernel<-adehabitatHR::kernelUD(one.sp.points)
  
  kernelTest50<-adehabitatHR::getverticeshr(onekernel, 50)
  kernelTest75<-adehabitatHR::getverticeshr(onekernel, 75)
  kernelTest95<-adehabitatHR::getverticeshr(onekernel, 95)
  
  
  
  p <-ggplot2::ggplot() + 
    ggplot2::geom_sf(data= sf::st_as_sf(kernelTest50), ggplot2::aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel)#, aes(fill = id, alpha = 0.5)
    )+
    ggplot2::geom_sf(data= sf::st_as_sf(kernelTest75), ggplot2::aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel) #, aes(fill = id, alpha = 0.5)
    )+
    ggplot2::geom_sf(data= sf::st_as_sf(kernelTest95), ggplot2::aes(alpha = 0.5), fill = unique(oneSpecies$colorKernel)#, aes(fill = id, alpha = 0.5)
    )+
    #geom_sf(data= sf::st_as_sf(one.grizzly.mcp), aes(fill = id, alpha = 0.5)) +
    #scale_fill_discrete(name = "Animal id")+
    #geom_point(data = xy.obs, aes(x=LONGITUDE , y=LATITUDE))+
    ggplot2::geom_sf(data = world) +
    ggplot2::coord_sf(crs = 4326, xlim = range(for_kernel$lon), ylim = range(for_kernel$lat)) +
    #annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
    #        color="black") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)) )+ 
    ggplot2::guides(alpha = "none", color = "none", size = "none")+
    ggplot2::ggtitle(i)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none")
  print(p)
  
}


trialspdf <- sf::st_as_sf(for_kernel, 
                       coords = c("lon","lat"),
                       crs = sf::st_crs(4326))
trialspdf <- as(trialspdf, "Spatial")

trialkernel<-adehabitatHR::kernelUD(trialspdf[,8])


image(trialkernel[[2]])

## not working without calling package, future fix no important
#plot(getverticeshr(trialkernel[[1]], 95), add = TRUE)

plot(sf::st_as_sf(adehabitatHR::getverticeshr(trialkernel[[1]], 95), add = TRUE))

# creating SpatialPolygonsDataFrame

kd_names <- names(trialkernel)

ud <- lapply(trialkernel, function(x) try(adehabitatHR::getverticeshr(x, 95)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- kd_names[i]
})
sdf_poly <- Reduce(rbind, ud)

terra::plot(terra::vect(sdf_poly))

ud2 <- lapply(trialkernel, function(x) try(adehabitatHR::getverticeshr(x, 70)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud2), function(i) {
  row.names(ud2[[i]]) <<- kd_names[i]
})

sdf_poly2 <- Reduce(rbind, ud2)

#not working well without the call of the library
plot(sf::st_as_sf(sdf_poly2))

ud3 <- lapply(trialkernel, function(x) try(adehabitatHR::getverticeshr(x, 50)))

# changing each polygons id to the species name for rbind call

sapply(1:length(ud3), function(i) {
  row.names(ud3[[i]]) <<- kd_names[i]
})
sdf_poly3 <- Reduce(rbind, ud3)

#not working well without the call of the library

plot(sf::st_as_sf(sdf_poly3))


library(adehabitatHR)
library(ggplot2)

df <- fortify(sdf_poly)
df2 <- fortify(sdf_poly2)
df3 <- fortify(sdf_poly3)

locIsland<-data.frame(y = -8.534234, x = -78.962968)

g <- ggplot() +
  geom_polygon(data=df3, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  geom_polygon(data=df2, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  geom_polygon(data=df, aes(x = long, y = lat, fill = id, group = group),alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  #coord_equal() +
  geom_sf(data = world) +
  ggplot2::geom_point(data =locIsland, ggplot2::aes(x = x, y = y), pch = 2, cex= 2)+
  coord_sf(xlim = range(for_kernel$lon), ylim = range(for_kernel$lat)) +
  theme_bw()+
  ggplot2::labs(fill = "Species")+
  xlab("Longitude")+
  ylab("Latitude")
g

ggplot2::ggsave(g, filename = "plots/kernel_dives_2019.png", bg="white", units = "in",
                height =4 ,
               width = 5, dpi = 300)
g + facet_wrap(~id)