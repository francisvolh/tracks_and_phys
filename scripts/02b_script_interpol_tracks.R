### incorporate interpolation into Track and Phys code

# test if interpolation full tracks or individual trips

# full tracks of with fixed (a couple of tracks with additional artificial fix) data already 

dep_dataPeru<-readRDS("data/dep_dataPeru_seabiRds.RDS")
dd <- dep_dataPeru$dep_id

gps.data_raw<-readRDS("data/gps_data_seabiRdsFIXEDv2.RDS")
unique(gps.data_raw$dep_id)

toMatch<-c("C06",  "C08")# get rid of birds that didnt go anywhere 
bye_bye <-grep(paste(toMatch, collapse = "|"), unique(gps.data_raw$dep_id), value=TRUE)

gps_data <- gps.data_raw |> 
  dplyr::filter(!dep_id %in% bye_bye) |> 
  dplyr::select(dep_id, time, lon, lat, coldist, dist, dt, speed
  )|>
  #dplyr::collect() |> 
  dplyr::arrange(dep_id, time) 

locs_sf <- sf::st_as_sf(gps_data, coords = c('lon', 'lat'), crs = 4326) ## I am not reprojecting because this is the tropics

#locs_sf <- sf::st_transform(locs_sf, crs = 5387) ## 

##################################################################
######################### PLOTTING ONLY ######################
tracks_sf <- locs_sf |> 
  dplyr::group_by(dep_id) |> 
  dplyr::summarize(
    tot_dist = sum(dist),
    n = dplyr::n(),
    do_union=FALSE) |> 
  dplyr::filter(n > 1) |> 
  sf::st_cast(to = 'LINESTRING')


turbo_pal <- c(viridis::turbo(n = length(dd), direction = -1)) # create a pallet of colour for all the dep_ids

col<-plyr::mapvalues(tracks_sf$dep_id,from=dd,to=turbo_pal) # create a vector for all the data to feed leaflet

leaflet::leaflet()|>
  leaflet::addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
    options = leaflet::tileOptions(variant='stamen_toner_lite', apikey = 'hidden') )|>
  leaflet::addPolylines(data = tracks_sf, color = col)

mapview::mapview(tracks_sf, zcol = 'dep_id') 
########################################################################################
########################################################################################

#Prep data for interpolatitracks_sf#Prep data for interpolation
# time step for interpolating the gps data


locs_sf <- sf::st_transform(locs_sf, crs = 5387) ## 


time_step <- '1 min'



# generate a list of prediction times for each individual in gps
predTime <- gps_data |> 
  dplyr::group_by(dep_id) |> 
  dplyr::summarize(
    start = lubridate::round_date(min(time), time_step),
    end = lubridate::round_date(max(time), time_step))

pt <- lapply(1:nrow(predTime), function(x) seq.POSIXt(predTime$start[x], predTime$end[x], time_step))

names(pt) <- predTime$dep_id


# Use crawl to interpolate gps data at time-stemp

lnError <- crawl::argosDiag2Cov(50,50,0) # 50m isotropic error ellipse - this assumes a conservative 50 m error in the GPS


# format data for crawl
crawlData <- data.frame(ID = gps_data$dep_id,
                        time = gps_data$time,
                        x = sf::st_coordinates(locs_sf)[,1],
                        y = sf::st_coordinates(locs_sf)[,2],
                        ln.sd.x = lnError$ln.sd.x,
                        ln.sd.y = lnError$ln.sd.y,
                        error.corr = lnError$error.corr)

pt <- vector('list', length(unique(crawlData$ID)))


names(pt) <- unique(crawlData$ID)

for (i in names(pt)) pt[[i]] <- lubridate::round_date(seq.POSIXt(min(crawlData$time[crawlData$ID == i]), 
                                                                 max(crawlData$time[crawlData$ID == i]), 
                                                                 time_step),
                                                      time_step)

# fit the correlated random walk model
set.seed(32154)
system.time({
  crwOut <- momentuHMM::crawlWrap(crawlData,
                                  theta = c(6.5,-.1),
                                  fixPar = c(1,1,NA,NA),
                                  err.model = list(x = ~ln.sd.x-1,
                                                   y = ~ln.sd.y-1,
                                                   rho = ~error.corr),
                                  predTime = pt, 
                                  retryFits = 0,
                                  attempts = 1520) # 400 attempts misses birds
}) #with 1500 it may do 55

plot(crwOut)

#saveRDS(crwOut, 'data/crwOutv3.RDS') # does 53 out of 56, a refit, 400 att
saveRDS(crwOut, 'data/crwOutv4.RDS') # 4000 att , 0 refit, 

unique(gps_data$dep_id)
unique(crwOut$crwPredict$ID)

setdiff(unique(gps_data$dep_id), unique(crwOut$crwPredict$ID))
# missing "A21PEBO_19112019_A104"
##################### run only the missing birds ######################
missing_birbs <- setdiff(streets, patterns)


gps.data_raw<-readRDS("data/gps_data_seabiRdsFIXEDv2.RDS")
unique(gps.data_raw$dep_id)


gps_data <- gps.data_raw |> 
  dplyr::filter(dep_id == "A21PEBO_19112019_A104") |> 
  dplyr::select(dep_id, time, lon, lat, coldist, dist, dt, speed
  )|>
  #dplyr::collect() |> 
  dplyr::arrange(dep_id, time) 

locs_sf <- sf::st_as_sf(gps_data, coords = c('lon', 'lat'), crs = 4326) ## I am not reprojecting because this is the tropics


locs_sf <- sf::st_transform(locs_sf, crs = 5387) ## 


time_step <- '1 min'



# generate a list of prediction times for each individual in gps
predTime <- gps_data |> 
  dplyr::group_by(dep_id) |> 
  dplyr::summarize(
    start = lubridate::round_date(min(time), time_step),
    end = lubridate::round_date(max(time), time_step))

pt <- lapply(1:nrow(predTime), function(x) seq.POSIXt(predTime$start[x], predTime$end[x], time_step))

names(pt) <- predTime$dep_id


# Use crawl to interpolate gps data at time-stemp

lnError <- crawl::argosDiag2Cov(50,50,0) # 50m isotropic error ellipse - this assumes a conservative 50 m error in the GPS


# format data for crawl
crawlData <- data.frame(ID = gps_data$dep_id,
                        time = gps_data$time,
                        x = sf::st_coordinates(locs_sf)[,1],
                        y = sf::st_coordinates(locs_sf)[,2],
                        ln.sd.x = lnError$ln.sd.x,
                        ln.sd.y = lnError$ln.sd.y,
                        error.corr = lnError$error.corr)

pt <- vector('list', length(unique(crawlData$ID)))


names(pt) <- unique(crawlData$ID)

for (i in names(pt)) pt[[i]] <- lubridate::round_date(seq.POSIXt(min(crawlData$time[crawlData$ID == i]), 
                                                                 max(crawlData$time[crawlData$ID == i]), 
                                                                 time_step),
                                                      time_step)

# fit the correlated random walk model
set.seed(32154)
system.time({
  crwOut <- momentuHMM::crawlWrap(crawlData,
                                  theta = c(6.5,-.1),
                                  fixPar = c(1,1,NA,NA),
                                  err.model = list(x = ~ln.sd.x-1,
                                                   y = ~ln.sd.y-1,
                                                   rho = ~error.corr),
                                  predTime = pt, 
                                  retryFits = 0,
                                  attempts = 1520) # 400 attempts misses birds
}) #with 1500 it may do 55

plot(crwOut)
saveRDS(crwOut, 'data/crwOutv4missing.RDS') # 4000 att , 0 refit, 



