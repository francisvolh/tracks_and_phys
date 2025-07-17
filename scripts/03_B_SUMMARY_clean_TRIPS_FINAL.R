#for chapter 2: foraging and metabolites



###############################################################################
############# Initial processing, skip to get fixed data with metrics #########
# get the data interpolated data without significant cleaning, from fixed data
# no C02PEBO, no G12GUCO2019, G12RLCO

# call interpolated data with 56 birds
gps.data.int <- readRDS("data/hmmDatav4Joint.RDS")

# TO SKIP if loading fixed data
#clean some birds out 
#gps.data<-gps.data |> 
# filter(dep_id != "C02PEBO_20191112_A150_S2") |> #sat on a rock for cam dep
#filter(dep_id != "G19RLCO_03 20180623-155726") |> #Chuita
#filter(dep_id != "G12GUCO_COR09_2019") #non functional deployed Ecotone COR09
#115980 rows

#seabirds deployment file
dep_dataPeru<-readRDS("data/dep_dataPeru_seabiRds.RDS")

#gps.data<-gps.data[which(gps.data$dep_id=="A21PEBO_19112019_A104"),] # select a  bird to check stuff
#gps.data<-gps.data[which(gps.data$time>"2019-11-18 16:57:06"),] # select a  bird to check stuff

#produce YEAR and SPECIES variables 



#produce YEAR and SPECIES variables 
#filter out very short bursts of GPS fixes for GUCOs (leaving the ones for PEBOs as are insignificant)

gps.data.int <- gps.data.int |>
  dplyr::mutate(
    species = substring(ID, 4, 7),
    time = lubridate::with_tz(time,tz = "America/Lima"), #### ACTIVATE FOR FINAL SUMMAGPS
    year = lubridate::year(time),
    day = lubridate::day(time)) 

#produce labels for in_trip and trip_id 
# First run with 1km, allowed to detect some issues
# (eg birds that stayed in the island nearby (1 "cheaters"), or "bathing trips"  (1 GUCO), 
# or similar. 
# these trips were discarded after (So the metrics are calculaled correctly, specially time and tot dist)

# one cheater last trip, was recalculated for C14PEBO 2019 to not miss the last trip and phys



gps.data <- gps.data.int |>
  dplyr::filter(!is.na(coldist2)) |>
  dplyr::group_by(ID) |>
  dplyr::arrange(ID, time)|> 
  dplyr::mutate(
    yeartrip = lubridate::year(time),
    in_trip = ifelse(coldist2 > 1000, 1, 0), #change if needed eventually to > than 1 km away
    trip_id = seabiRds::getSessions(in_trip, ignore = TRUE, ignoreValue = 0), 
    unique_trip = paste0(ID,"_trip_",trip_id)
  )

# we will save this gps points with unique trips IDs after cleaning the one bird for 3.9km threshold instead of 1km

#######################################
#######################################
# GPS data is hmmDatav4Joint.RDS for interpolated fixed data
#######################################


#summarise trips without discarding specific trips, at 1km of colony
SUMMAGPS <- gps.data |>
  dplyr::filter(!is.na(trip_id)) |>
  dplyr::group_by(ID,trip_id) |>
  dplyr::summarise(
    steps = dplyr::n(), #number of gps points that define the trip
    Year = max(year), #year of the trip
    Day = max(day), #day of the trip
    maxdist = max(coldist2)/1000, #max distance to the colony
    tottime = dplyr::n()/60, #sum(dt, na.rm = TRUE), #total time traveled (beware its a SUM of dif times between steps)
    totdist = sum(step, na.rm = TRUE)/1000, # total distance traveled (beware distance between steps)
    startt = min(time), #start of the trip
    endt = max(time),
    sampRTime= mean( dplyr::n(), na.rm=TRUE),
    TimeTrip = difftime(endt, startt, units = c("hours")),#end of the trip, This metric is now exactly the same as the data is interpolated
    minDiffTime = min( dplyr::n(), na.rm=TRUE),
    maxDiffTime=max( dplyr::n(), na.rm=TRUE), #largest sampling rate of trip
    MaxSpeed = max(speed, na.rm=TRUE),
    Spec = unique(species),
    unique_trip = unique(unique_trip),
    distCOLend = tail(coldist2,1),
    distCOLstart = head(coldist2,1),
    sampRFixesxMaxd = (steps/maxdist)
  ) 

#View(SUMMAGPS)
#nrow(SUMMAGPS)
summary(SUMMAGPS)
# 383 trips now, with interpolated data, at 1km, 
#needs to kill short trips below 5km, and reclasiffy one last trip for
# C14PEBO

####################################################################
####################################################################
################### Viz for all classified trips ###################


plot_list<-NULL
for (i in unique(SUMMAGPS$ID)) {
  p<-gps.data|>
    dplyr::filter(ID == i)|>
    ggplot2::ggplot(ggplot2::aes(x=time, y = coldist2))+
    #ggplot2::geom_point(data = gps.data_raw[which(gps.data_raw$dep_id == i),],
    #                   ggplot2::aes(x = time, y = (1000*coldist)), col = "red")+
    ggplot2::geom_line(ggplot2::aes(color =unique_trip ))+
    ggplot2::theme(legend.position = "none")+
    ggplot2::ggtitle(i)
  plot_list[[i]]<-p
}

save_plot_id<-cowplot::plot_grid(plotlist = plot_list, nrow =7, ncol =8 )
ggplot2::ggsave(save_plot_id, filename="plots/save_plot_IDsv1.png" ,height = 20, width = 30, dpi = 300)
####################################################################
####################################################################
# get last trip for C14 which is an overnight trip at GS Island (3.8km), 
# and re classified at 3.9km

unique(SUMMAGPS$ID)
unique(SUMMAGPS[which(SUMMAGPS$ID == "C14PEBO_20191121_A161_S1"),]$unique_trip)


one_bird_gps <-  gps.data.int |>
  dplyr::filter(!is.na(coldist2)) |>
  dplyr::group_by(ID) |>
  dplyr::arrange(ID, time)|> 
  dplyr::mutate(
    yeartrip = lubridate::year(time),
    in_trip = ifelse(coldist2 > 3900, 1, 0), #change if needed eventually to > than 1 km away
    trip_id = seabiRds::getSessions(in_trip, ignore = TRUE, ignoreValue = 0), 
    unique_trip = paste0(ID,"_trip_",trip_id)
  )|>
  dplyr::filter(ID == "C14PEBO_20191121_A161_S1")

one_bird <- one_bird_gps|>
dplyr::filter(ID == "C14PEBO_20191121_A161_S1")|>
  dplyr::filter(!is.na(trip_id)) |>
  dplyr::group_by(trip_id) |>
  dplyr::summarise(
    steps = dplyr::n(), #number of gps points that define the trip
    Year = max(year), #year of the trip
    Day = max(day), #day of the trip
    maxdist = max(coldist2)/1000, #max distance to the colony
    tottime = dplyr::n()/60, #sum(dt, na.rm = TRUE), #total time traveled (beware its a SUM of dif times between steps)
    totdist = sum(step, na.rm = TRUE)/1000, # total distance traveled (beware distance between steps)
    startt = min(time), #start of the trip
    endt = max(time),
    sampRTime= mean( dplyr::n(), na.rm=TRUE),
    TimeTrip = difftime(endt, startt, units = c("hours")),#end of the trip, This metric is now exactly the same as the data is interpolated
    minDiffTime = min( dplyr::n(), na.rm=TRUE),
    maxDiffTime=max( dplyr::n(), na.rm=TRUE), #largest sampling rate of trip
    MaxSpeed = max(speed, na.rm=TRUE),
    Spec = unique(species),
    unique_trip = unique(unique_trip),
    distCOLend = tail(coldist2,1),
    distCOLstart = head(coldist2,1),
    sampRFixesxMaxd = (steps/maxdist)
  ) 

View(one_bird)
one_trip <- SUMMAGPS|>
  dplyr::filter(ID != "C14PEBO_20191121_A161_S1_trip_7" )

# get the last trip for this bird
one_trip <- dplyr::filter(one_bird, trip_id == 5)

one_trip$ID <- "C14PEBO_20191121_A161_S1"

#relabel trip as 7
one_trip$trip_id <- 7
one_trip$unique_trip <- "C14PEBO_20191121_A161_S1_trip_7"

View(one_trip)
### for gps data with trip Id df

#filter out C14PEBO and last trip points
SUMMAGPS|>
  dplyr::filter(unique_trip == "C14PEBO_20191121_A161_S1_trip_7")|>
  dplyr::pull(startt)

#exclude the gps points labelled as trip 7 for C14PEBO at 1km
gps.data_clean <- gps.data |>
  dplyr::filter(unique_trip != "C14PEBO_20191121_A161_S1_trip_7")

# pull the gps for C14PEBO classified at 3.9km after the previous breakpoint

cleanning <- one_bird_gps |>
  dplyr::filter(time >"2019-11-20 18:55:00 -05")
unique(cleanning$unique_trip)


cleanning$unique_trip[cleanning$unique_trip == 'C14PEBO_20191121_A161_S1_trip_5'] <- "C14PEBO_20191121_A161_S1_trip_7"
View(cleanning)

gps.data_clean<-rbind(gps.data_clean, cleanning)



saveRDS(gps.data_clean, "data/gps.data_clean_INT.RDS") # last save just in case after totdist fix (was in meters, now all is kms) July 16th 2025


##################################################
# remove the 1 trip in SUMMAGPS for the one bird
SUMMAGPS<-SUMMAGPS|>
  dplyr::filter(ID != "C14PEBO_20191121_A161_S1" | trip_id !=7)

#bind the one trip at 3.9km to all the trips summary
SUMMAGPS_clean<-rbind(SUMMAGPS, one_trip)
summary(SUMMAGPS_clean)
View(SUMMAGPS_clean)

#check outliers or strange trips
#plot(SUMMAGPS$maxdist,SUMMAGPS$tottime) # includes a trip with huge duration, will exclude

#filter out outliers: 
# 70 hour trip, is Gap of one GPS failing (G04PEBO 2019)
# anything over 10 hours are overnight trips:
# G24PEBO2019 Chao Island
# C14PEBO 2019 G Sur Island
# A16PEBO 2019 Chao island

#clean out trips with more than 36 hours (failed gps in mid deployment) 
SUMMAGPS_clean <- SUMMAGPS_clean[which(SUMMAGPS_clean$tottime <= 36),]

# also removed very short trips, less than 5km distance
SUMMAGPS_clean <- SUMMAGPS_clean[which(SUMMAGPS_clean$maxdist >=5),]


#nrow(SUMMAGPS)
#[1] 334 # includes two overnight trips, will keep for firs iter or stats

#saveRDS(SUMMAGPS_clean, "data/SUMMAGPS_clean.RDS") #last save on July 16th 2025
                                                    # after the fix of totdist in meters to kms

#summary of number of trips
SUMMAGPS_clean<-readRDS("data/SUMMAGPS_clean.RDS")

SUMMAGPS_clean|>
  dplyr::group_by(Spec, Year)|>
  dplyr::summarise(
    Total = dplyr::n()
  )


a<-SUMMAGPS_clean |>
  dplyr::filter(Spec == "GUCO") |>
  dplyr::group_by(ID) |> 
  dplyr::filter(trip_id == min(trip_id)) #closest to sampling for GUCO

b<-SUMMAGPS_clean |>
  dplyr::filter(Spec == "PEBO") |>
  dplyr::group_by(ID) |> 
  dplyr::filter(trip_id == max(trip_id)) #closest to sampling for PEBO

SUMMAGPS_1trip<-rbind(a,b)
View(SUMMAGPS_1trip)
#saveRDS(SUMMAGPS_1trip, file = "data/SUMMAGPS_1tripFULL_int_v2.RDS") #only 1 trip per bird

## the code produced Summary for metrics per metrics at 1km, cleans trips below 5
## one bird C14PEBO has one trip (last) classified at 3.9, (overnight in island nearby) 

