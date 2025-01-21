## chunk to include in Summary cleand or last stop to interpolate tracks

#read interpolated gps data
##########################################################################
##########################################################################
######################## merge two interpolations     ####################

crawl1<- readRDS('data/crwOutv4.RDS')
ids1<-unique(crawl1$crwPredict$ID)

crawl2<- readRDS('data/crwOutv4missing.RDS')
ids2<-unique(crawl2$crwPredict$ID)
ids <-c(ids1, ids2)

#plot(crawl2)

head(crawl2)

# read in deployments
deployments <- readRDS('data/dep_dataPeru_seabiRds.RDS') |> 
  dplyr::filter(dep_id  %in% ids)|>
  dplyr::select(dep_id, metal_band, site, nest, time_released, time_recaptured, 
                dep_lon, dep_lat, mass_on, mass_off, status_on, status_off) |>
  dplyr::filter( !stringr::str_detect(dep_id, 'RLCO') ) # exclude red legged


col_loc <- unique(deployments[, c('site', 'dep_lon', 'dep_lat')])

col_loc <- sf::st_as_sf(col_loc, coords = c('dep_lon', 'dep_lat'), crs = 4326) ## I am not reprojecting because this is the tropics

col_loc <- sf::st_transform(col_loc, crs =5387 ) #things explode when reprojecting for the centroids after the crawl and prepData


# prep the data using momentuHMM and produce new estimated distance to colony 

system.time({
  hmmData1 <- momentuHMM::prepData(crawl1,
                                  centers =as.matrix(sf::st_coordinates(col_loc)))
})

# rename new dist to col
hmmData1$coldist2 <- hmmData1$center1.dist

saveRDS(hmmData1, "data/hmmDatav4.RDS") 

# prep the data using momentuHMM and produce new estimated distance to colony 

system.time({
  hmmData2 <- momentuHMM::prepData(crawl2,
                                   centers =as.matrix(sf::st_coordinates(col_loc)))
})

# rename new dist to col
hmmData2$coldist2 <- hmmData2$center1.dist
saveRDS(hmmData2, "data/hmmDatav3missing.RDS") # 


hmmDataJoint <- rbind(hmmData1, hmmData2)
length(unique(hmmDataJoint$ID)) # check there are 56 birds

rm(hmmData1, hmmData2)
saveRDS(hmmDataJoint, "data/hmmDatav4Joint.RDS") # 
hmmData<-hmmDataJoint

hmmData <- readRDS("data/hmmDatav4Joint.RDS")
gps.data.int<-hmmData |>
  dplyr::mutate(
    species = substring(ID, 4, 7),
    time = lubridate::with_tz(time,tz = "America/Lima"), #### ACTIVATE FOR FINAL SUMMAGPS
    year = lubridate::year(time),
    day = lubridate::day(time)) 

head(hmmData)
gps.data_raw<-readRDS("data/gps_data_seabiRdsFIXEDv2.RDS")
gps.data_raw<-gps.data_raw |>
  dplyr::mutate(
    species = substring(dep_id, 4, 7),
    time = lubridate::with_tz(time,tz = "America/Lima"), #### ACTIVATE FOR FINAL SUMMAGPS
    year = lubridate::year(time),
    day = lubridate::day(time)) 

plot_list<-list()
for (i in unique(hmmData$ID)) {
  
  p<-hmmData|>
    dplyr::filter(ID == i)|>
    ggplot2::ggplot(ggplot2::aes(x=time, y = coldist2))+
    ggplot2::geom_point(data = gps.data_raw[which(gps.data_raw$dep_id == i),],
                        ggplot2::aes(x = time, y = (1000*coldist)), col = "red")+
    ggplot2::geom_line()+
    
    ggplot2::ggtitle(i)
  #plot(p)
  plot_list[[i]]<-p
 # readline('next')
}
save_plot<-cowplot::plot_grid(plotlist = plot_list, nrow =7, ncol =8 )
ggplot2::ggsave(save_plot, filename="plots/save_plot_gridv4ALLnewtime.png" ,height = 30, width = 40, dpi = 300)


### one G04 pebo forced trip still may have a issue, but saved the one mid trip (trip before may be 3 days)
## cut trips last 5 trips from G10_GUCO_COR01 ????
# check other trips that had tips interpolated: too far or okay?