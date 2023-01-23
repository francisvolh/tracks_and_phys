#for chapter 2: foraging and metabolites
#load packages
library(tidyverse)
library(seabiRds)
library(GGally)

#############
#get the data RAW, without significant cleaning
#gps.data<-readRDS(file="C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")

#get data Fixed: no Chuita, no null deployment COR09, 2 points added for 2 last 1-trips
#ALSO BELOW, SO SKIP all the following code to produce unique_trip_ids
gps.data<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#seabirds deployment file
dep_dataPeru<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")


# TO SKIP if loading fixed data
#get rid of the one Chuita
gps.data<-gps.data[which(gps.data$dep_id!="G19RLCO_03 20180623-155726"),]
#get rid of non deployed COR
gps.data<-gps.data[which(gps.data$dep_id!="G12GUCO_COR09_2019"),]

#get rid of bird that sat on a rock for cam deployment
gps.data<-gps.data[which(gps.data$dep_id!="C02PEBO_20191112_A150_S2"),]


#gps.data<-gps.data[which(gps.data$dep_id=="A21PEBO_19112019_A104"),]
#gps.data<-gps.data[which(gps.data$time>"2019-11-18 16:57:06"),]

#produce YEAR and SPECIES variables 
#filter out very short bursts of GPS fixes for both species
gps.data <- gps.data %>% 
  filter(!(dt <= (1/120)))


#clean data again with speeds less than 80km/h (with seabiRds)
gps.data <- cleanGPSData(data = as.data.frame(gps.data),
                               deployments = dep_dataPeru,
                                speedThreshold = 110, ##ACTIVATE for FINAL SUMMAGPS
                               plot = FALSE) #81 km/h treshold kills about 37 fast points

#produce YEAR and SPECIES variables 
#filter out very short bursts of GPS fixes for GUCOs (leaving the ones for PEBOs as are insignificant)

gps.data <- gps.data %>% 
  mutate(
    species = substring(dep_id, 4, 7),
    #time = lubridate::with_tz(time,tz = "America/Lima"), #### ACTIVATE FOR FINAL SUMMAGPS
    year = lubridate::year(time),
    day = lubridate::day(time))# %>%  
  #filter(!(dt <= (1/120) & species == "GUCO"))



#produce labels for in_trip and trip_id ONLY without cleaning ANYTHING
# at 1 km

gps.data <- gps.data %>% 
  filter(!is.na(coldist))%>%
  group_by(dep_id) %>% 
  arrange(dep_id, time) %>% 
  mutate(
    yeartrip = lubridate::year(time),
    in_trip = ifelse(coldist > 1, 1, 0), #change if needed eventually to > than 1 km away
    trip_id = getSessions(in_trip, ignore = T, ignoreValue = 0), 
    unique_trip = paste(dep_id,"_trip_",trip_id)
  )

#saveRDS(gps.data,"C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")
#gps.data<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#summarise trips without cleaning, at 1km of colony
SUMMAGPS <- gps.data %>% 
  filter(!is.na(trip_id))%>% 
  group_by(dep_id,trip_id) %>%
  dplyr::summarise(
    steps = n(), #number of gps points that define the trip
    Year = max(year), #year of the trip
    Day = max(day), #day of the trip
    maxdist = max(coldist), #max distance to the colony
    tottime = sum(dt, na.rm = TRUE), #total time traveled (beware its a SUM of dif times between steps)
    totdist = sum(dist, na.rm = TRUE), # total distance traveled (beware distance between steps)
    startt = min(time), #start of the trip
    endt = max(time),
    sampRTime= mean(dt, na.rm=TRUE),
    TimeTrip = difftime(endt, startt, units = c("hours")),#end of the trip
    minDiffTime = min(dt, na.rm=TRUE),
    maxDiffTime=max(dt, na.rm=TRUE), #largest sampling rate of trip
    MaxSpeed = max(speed, na.rm=TRUE),
    Spec = unique(species),
    unique_trip = unique(unique_trip),
    distCOLend = tail(coldist,1),
    distCOLstart = head(coldist,1),
    sampRFixesxMaxd = (steps/maxdist)
  ) 

#View(SUMMAGPS)
#nrow(SUMMAGPS)
#[1] 392 #without removing any weird data
#[1] 389 # removing speeds greater than 80 km/h points from seabiRds

#check outliers or strange trips
#plot(SUMMAGPS$maxdist,SUMMAGPS$tottime) # includes a trip with huge dt
#plot(SUMMAGPS$maxdist,SUMMAGPS$TimeTrip)

#only to CHECK 
#before getting rid of short trips, find depths
#DDD<-SUMMAGPS[which(SUMMAGPS$maxdist <= 5),]
#plot(DDD$maxdist,DDD$TimeTrip)
#unique(DDD$dep_id)
#unique(DDD$unique_trip)
#write.csv(DDD, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/DDD.csv")

#tdrdata<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/technoTDRPeru_data_NEW.RDS")
#allTDR<-tdrdata

#tdrdata <- tdrdata %>% 
 # filter(dep_id %in% unique(DDD$dep_id)) %>% 
  #mutate(
   # time = lubridate::with_tz(time,tz = "America/Lima")
#  )

#depthIDs <- unique(tdrdata$dep_id)

#XXX <- gps.data[which(gps.data$dep_id %in% unique(DDD$dep_id)),]

#YYY <- merge(XXX, tdrdata, all=TRUE )

#maxDepthtrips <- NULL
#subsetTDR<-NULL
#sub<-NULL

#rowslength <- NULL
#DDD <- DDD[which(DDD$dep_id %in% depthIDs),]

#for (i in 1:nrow(DDD)){
 
  #get values from summary table
  #sub <- DDD[i, ]
  
   #depid <- unique(sub$dep_id)
   #startT <- unique(sub$startt)
   #endT <- unique(sub$endt)
   #uniqueTRIPid<-unique(sub$unique_trip)
  
  #subset data using values from Summary Table
 # subsetTDR <- tdrdata[which(tdrdata$dep_id == depid),]
  
#  subsetTDR <- subsetTDR[which(subsetTDR$time >= startT & subsetTDR$time <= endT), ]

  #to map it
 # GPSsubset<-gps.data[which(gps.data$unique_trip == uniqueTRIPid),]
  #GPSsubset <- GPSsubset[order(GPSsubset$time),]
  
  #subsetTDR <- merge(subsetTDR, GPSsubset, all = TRUE)
    
  #subsetTDR$lon <- na_interpolation(subsetTDR$lon)
  #subsetTDR$lat <- na_interpolation(subsetTDR$lat)
  
   # toplot<-subsetTDR
    #p <- ggplot() +
     # geom_point(data = toplot, aes(x = lon, y = lat , color = depth), cex= 1) +
     #scale_colour_gradient(low = "#132B43", high = "#56B1F7") +
      #xlab("Longitude")+
      #ylab("Latitude")+
      #geom_sf(data = world, aes()) +#+ #add basemap
      #coord_sf(crs = 4326, xlim = range(toplot$lon), ylim = range(toplot$lat)) +
      #theme(legend.position = "none")+
      #ggtitle(uniqueTRIPid)
    
    #print(p)
    #readline('next')
  
  
  #MaxDepth <- max(subsetTDR$depth, na.rm = TRUE) #get the max depth value for that specific trip using dep_id and time of trip
  #subsetrows<-nrow(subsetTDR)
  
  #Join the results
 # maxDepthtrips <- rbind(maxDepthtrips, MaxDepth)
#}

#uniquesIDs<-DDD[,c("unique_trip")]
#steplength<-DDD[,c("steps")]
#depthTRIPS<-cbind(maxDepthtrips,uniquesIDs, steplength)
#View(depthTRIPS)
#row.names(depthTRIPS)<-NULL

#check how many "trips" with 1 to 5 gps points
#View(SUMMAGPS[which(SUMMAGPS$steps <= 5),])

#AAA<-SUMMAGPS[which(SUMMAGPS$steps <= 5),]
#nrow(AAA)

#plot(AAA$maxdist,AAA$tottime)
#plot(AAA$maxdist,AAA$TimeTrip)

#write.csv(AAA, "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/AAAi.csv") ###AAAi is after cleaning speeds greater than 80 km/h



#clean out trips less than 5 GPS points MAY NEED TO INCREASE for RST
SUMMAGPS <- SUMMAGPS[which(!SUMMAGPS$steps <= 10),]

#nrow(SUMMAGPS)
#[1] 330 # after cleaning 1 pointers and greater than 80km/h speeds

SUMMAGPS <- SUMMAGPS[which(!(SUMMAGPS$distCOLend > 10 & SUMMAGPS$sampRFixesxMaxd <0.7 )),]

#View(SUMMAGPS[which(SUMMAGPS$TimeTrip >30),]) # trip with a gap at the beginning, 
#GOT RID OF THE ISSUE by calculating time of trip with start and begin of trip, not dt sum 
#SUMMAGPS<-SUMMAGPS[which(!SUMMAGPS$tottime >30),] #to clip trip put, not needed though

#View(SUMMAGPS[which(SUMMAGPS$TimeTrip > 10 ),])
#BBB<-SUMMAGPS[which(SUMMAGPS$TimeTrip > 10),]
#write.csv(BBB, "BBB.csv")

#detect SHORT trips to be deleted , confirm with DIVES if possible
#checkDives <- SUMMAGPS[which(SUMMAGPS$maxdist < 5 ),] # to see if there are DIVES here in the axxys
#get rid of trips below 5km as not considered foraging
SUMMAGPS <- SUMMAGPS[which(!SUMMAGPS$maxdist < 5 ),]


#ONLY for shorts version
#get rid of all trips greater than 10 hours (will delete trip in which birds stayed at Chao and GS overnight)
#SUMMAGPSshort <- SUMMAGPS[which(!SUMMAGPS$TimeTrip > 10 ),]

#nrow(SUMMAGPS)
#[1] 326 after cleaning overnight trips to other islands

#plot(SUMMAGPS$maxdist,SUMMAGPS$TimeTrip)




#setwd("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/")
#write.csv(checkDives, "checkDives.csv")

#indeces NOT NEEDED
#SUMMAGPS <- SUMMAGPS %>% 
#mutate(
 # ind_TL_DUR = maxdist/as.numeric(TimeTrip),
  #ind_TD_DUR = totdist/as.numeric(TimeTrip)
#)

#write.csv(SUMMAGPS, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv") #file without 1-5 pointer trips, no outliers in duration, NO trips less than 5km from Col
#write.csv(SUMMAGPSshort, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPSshort.csv") #file without 1-5 pointer trips, no outliers in duration, NO trips less than 5km from Col


a<-SUMMAGPS %>%
  filter(Spec == "GUCO") %>%
  group_by(dep_id) %>% 
  filter(trip_id == min(trip_id))

b<-SUMMAGPS %>%
  filter(Spec == "PEBO") %>%
  group_by(dep_id) %>% 
  filter(trip_id == max(trip_id))

SUMMAGPS_1trip<-rbind(a,b)

#saveRDS(SUMMAGPS_1trip, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1tripFULL.RDS") #only 1 trip per bird

SUMMAGPS_1tripFULL #all trips including overnight
SUMMAGPS_1trip # excluding overnights

##############################################################################################################################
#gps.data<-gps.data[!is.na(gps.data$lat),]

##############################################################################################################################
#####################################################################################################################



#########
#RST clasification
# load C function, secondary functions, plotting functions
#dyn.load("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/scripts/residence space time/RST_residenceRadii.dll") # if using Windows7
#source("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/scripts/residence space time/RST_functions_all.R")

#origData <- gps.data


##GUCOs
#origData <- origData %>% 
 # filter(species == "GUCO")

#origData <- origData %>% 
 # filter(species == "PEBO")

#dataset <- data.frame(	"band" = origData$dep_id,
 #                     "lat" = origData$lat,
  #                    "lon" = origData$lon,
   #                   "datetime" = strptime(origData$time, format = "%Y-%m-%d %H:%M:%S"
    #                  ))


# remove NA lon/lat rows
#dataset <- dataset[!is.na(dataset$lon) & !is.na(dataset$lat), ]

# create grid x- and y- coordinates from the longitude/latitude coordinates
#library(mapproj)
#lambert <- mapproject(dataset$lon, dataset$lat, projection = "lambert", parameters = c(mean(dataset$lon), mean(dataset$lat)))
#scale <- haversineDist(min(dataset$lon), min(dataset$lat), max(dataset$lon), max(dataset$lat)) / projectDist(min(lambert$x), min(lambert$y), max(lambert$x), max(lambert$y), 1)
#dataset$x <- lambert$x * scale
#dataset$y <- lambert$y * scale



# --------------------- CALCULATING RESIDENCE VALUES --------------------- #
# create a time array: duration of trip in given time units
#time_units = "mins" # one of "secs", "mins", "hours", "days", "weeks"

# set the desired radius and threshold values
#radius <- c(0.2, 0.8332, 1.25, 3)

#threshold <- rep(0, length(radius))

# determine the different individuals
#bandIDs <- unique(dataset$band)

# for each track, calculate residence values
#all_tracks = data.frame()

#for (i in 1:length(bandIDs)) {
 # subdata = dataset[dataset$band == bandIDs[i], ]
  
  #subdata$time_diff = as.numeric(subdata$datetime - subdata$datetime[1], units = time_units)
  
  #result <- residenceCalc(subdata$x, subdata$y, subdata$time_diff, radius, threshold)
  #subdata = cbind(subdata, result)
  #all_tracks = rbind(all_tracks, subdata)
#}


#merge the RST classification column to GPS DATA seabiRds original  
#GUCOsubs <- all_tracks[,c("band", "datetime", "res_rad_0.8332_th_0")] #select only the 1 column and dep_id and time to merge


#rename columns back to sebiRds formatting
#GUCOsubs <- data.frame(	"dep_id" = GUCOsubs$band,
 #                            "time" = GUCOsubs$datetime,
  #                           "res_rad_0.4166_th_0" = GUCOsubs$res_rad_0.8332_th_0)


#merge the RST classification column to GPS DATA seabiRds original  
#PEBOsubs <- all_tracks[,c("band", "datetime", "res_rad_0.4166_th_0")] #select only the 1 column and dep_id and time to merge


#rename columns back to sebiRds formatting
#all_tracksSUB <- data.frame(	"dep_id" = all_tracksSUB$band,
 #                       "time" = all_tracksSUB$datetime,
  #                      "res_rad_0.4166_th_0" = all_tracksSUB$res_rad_0.4166_th_0)

#join PEBO and GUCO RST subsets, WHEN splitting
#all_tracksSUB <- rbind(PEBOsubs, GUCOsubs)


#write.csv(all_tracksSUB, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/CLASS_RST_0.8332.csv")
#write.csv(all_tracksSUB, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/CLASS_RST_0.4166.csv")

#LOAD RST with 0.4166 radius for both
#all_tracksSUB<-read.csv(file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/CLASS_RST_0.4166.csv")

#LOAD RST with 0.4166 for PEBO and 0.8322 for GUCO
#all_tracksSUB<-read.csv(file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/CLASS_RST_0.8332.csv")


#do the merge
#merged.GPS<-merge(gps.data, all_tracksSUB, by.x = c("dep_id", "time"), by.y=c("band", "datetime"), all=TRUE)

#check 1 bird G15GUCO_13_20181129-120701 DOS NOT has speed below 5km/h in trip
#merged.GPS  <- merged.GPS %>% 
 # filter(coldist > 1) %>% #clearing away from COLONY keeps only  traveled distances
#  mutate(
 #   activity = case_when(res_rad_0.4166_th_0 == 0  ~ "travel",
  #                       res_rad_0.4166_th_0 > 0 ~ "foraging", 
   #                      res_rad_0.4166_th_0 < 0 ~ "resting")  
  #) # 0 is traveling, > 0 is foraging, and less than 0 is resting   



#WWW<-merged.GPS %>% 
 # filter(dep_id == "G13GUCO_COR08_2019") 

#WWW %>% 
#  group_by(trip_id) %>% 
#  summarise(
#    minSpeed = min(speed, na.rm = TRUE),
#    maxSpeed = max(speed, na.rm = TRUE),
#    meanSpeed = mean(speed, na.rm = TRUE),
#    active = max(res_rad_0.4166_th_0),
#    .groups = "drop"
#  ) 
  
#SUMMARY of trips with the RST classification
#SUMMATRIPS <- merged.GPS %>% 
 # filter(!is.na(trip_id)) %>% 
  #group_by(dep_id,trip_id) %>%
  #dplyr::summarise(
   # steps = n(), #number of gps points that define the trip
#    Year = max(year), #year of the trip
 #   Day = max(day), #day of the trip
  #  maxdist = max(coldist), #max distance to the colony
   # tottime = sum(dt, na.rm = TRUE), #total time traveled (beware its a SUM of dif times between steps)
#    totdist = sum(dist, na.rm = TRUE), # total distance traveled (beware distance between steps)
 #   startt = min(time), #start of the trip
  #  endt = max(time),
   # TimeTrip = difftime(endt, startt, units = c("hours")),#end of the trip
    #maxDiffTime=max(dt, na.rm=TRUE), #largest sampling rate of trip
    #minSpeed = min(speed, na.rm = TRUE),
    #maxSpeed = max(speed, na.rm = TRUE),
    #meanSpeed = mean(speed, na.rm = TRUE),
    #Spec = unique(species),
    #unique_trip = unique(unique_trip),
    #active = max(res_rad_0.4166_th_0),
    #.groups = "drop"
#  )
#View(SUMMATRIPS)

#nrow(SUMMATRIPS[which(SUMMATRIPS$active == 0),]) # trips that HAVE no Foraging according to RST

#preSUBRST<-RRR #previous classification with 50 trips as no foraging

#RRR<-(SUMMATRIPS[which(SUMMATRIPS$active == 0),])#new classification with only 30 trips as no foraging 
#only 3 GUCOs now
#View(RRR)
#View(RRR[which(RRR$Spec =="GUCO"),])
#dep_idsnoFor<-unique(RRR[which(RRR$Spec =="GUCO"),]$dep_id)

#unique(RRR$dep_id)
#unique(RRR$unique_trip)

#summary(gps.data[which(gps.data$dep_id %in% dep_idsnoFor),]$dt)


#clean out trips less than 5 GPS points MAY NEED TO INCREASE for RST
#SUMMATRIPS <- SUMMATRIPS[which(!SUMMATRIPS$steps <= 5),]
#SUMMATRIPS <- SUMMATRIPS[which(!SUMMATRIPS$TimeTrip > 10 ),]

##################
#assign bout ID, numeric foraging state
#bouts.GPS <- merged.GPS %>% 
 # filter(!is.na(activity)) %>% 
#  group_by(unique_trip) %>% 
 # mutate(
  #  activity2 = case_when(activity == "travel"  ~ 0, 
    #                      activity ==  "foraging" ~ 1,
     #                     activity == "resting" ~ 0),
   # bout_id = getSessions(activity2, ignore = T, ignoreValue = 0)
  #)


### NEED TO SUMMARY BOUTS
###
### to work on this
####
### make summary with times foraging per trip for foraging effort metric
#SUMMABOUTS <- bouts.GPS %>% 
 # filter(!is.na(trip_id)) %>% 
  #filter(!is.na(bout_id))%>% 
  #group_by(dep_id,trip_id, bout_id) %>%
  #dplyr::summarise(
   # Year = max(year),
    #Day = max(day),
    #steps = n(),#how many gps points per bout
    #startBout = min(time),
    #endBout = max(time),
    #maxdistBout = max(coldist),
    #timeBout = as.numeric(difftime(endBout, startBout, units = 'hours')),
    #totdist = sum(dist, na.rm = TRUE),
    #maxDiffTime=max(dt, na.rm=TRUE),
    #unique_tripID = unique(unique_trip),
    #.groups = "drop"
  #)
#View(SUMMABOUTS)
#hist(SUMMABOUTS$timeBout)
#check bouts that are very long
#nrow(SUMMABOUTS[which(SUMMABOUTS$timeBout == 0),])
#nrow(SUMMABOUTS[which(SUMMABOUTS$timeBout > 0.75),])

#forHigh0.75<-YYY#before the subset of species 

#YYY<-(SUMMABOUTS[which(SUMMABOUTS$timeBout > 0.75),])#bouts with MORE than 0.75 hour
#nrow(YYY)
#unique(YYY$unique_tripID) # unique trips ID for bouts with MORE than 0.75 hour
#hist(YYY$timeBout)

#SUMMABOUTSperTRIP <- SUMMABOUTS %>% 
 # group_by(unique_tripID) %>%
  #dplyr::summarise(
   # Year = max(Year),
    #Day = max(Day),
    #steps = n(),#how many bouts
    #maxdistBout = max(maxdistBout),
    #TotTimeBouts = sum(timeBout),
    #totdistBouts = sum(totdist, na.rm = TRUE),
    #.groups = "drop"
 # )
#nrow(SUMMABOUTSINDS)


#SUMMA_b_and_t <- merge(SUMMATRIPS, SUMMABOUTSperTRIP, by.y = c("unique_tripID"), by.x = c("unique_trip"), all=TRUE)

#work <- SUMMA_b_and_t %>% 
 # filter(!is.na(dep_id)) %>% 
  #filter(!is.na(TotTimeBouts)) %>% 
  #mutate(
   # ind_TL_DUR = maxdist/as.numeric(TimeTrip),
    #ind_TD_DUR = totdist/as.numeric(TimeTrip)
#  )


#check how good the index correlates with bouts durations
#cor((1/work$ind_TL_DUR), work$TotTimeBouts)##TOTAL TRIP DURATION AND MAX DIST, correl with Tot Time Bouts
#cor(work$ind_TL_DUR, work$TotTimeBouts)

#cor((1/work$ind_TD_DUR), work$TotTimeBouts)
#cor(work$ind_TD_DUR, work$TotTimeBouts)

#is.na(work$TotTimeBouts)

#View(work[which(work$ind_TL_DUR >25),]) #may need to do interpolate that Allison suggested
#G09GUCO_COR04_2019 _trip_ 22

#work<-work[which(!work$TotTimeBouts>4),] #may need to do interpolate that Allison suggested


