#for chapter 2: foraging and metabolites
#load packages
library(dplyr)
library(seabiRds)
library(GGally)

#############
#get the data RAW, without significant cleaning
#gps.data<-readRDS(file="C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")
#118156 rows

# TO SKIP if loading fixed data
#clean some birds out 
#gps.data<-gps.data %>% 
 # filter(dep_id != "C02PEBO_20191112_A150_S2") %>% #sat on a rock for cam dep
  #filter(dep_id != "G19RLCO_03 20180623-155726") %>% #Chuita
  #filter(dep_id != "G12GUCO_COR09_2019") #non functional deployed Ecotone COR09
#115980 rows

#get data Fixed: no Chuita, no null deployment COR09, 2 points added for 2 last 1-trips
#ALSO BELOW, SO SKIP all the following code to produce unique_trip_ids
gps.data<-readRDS("data/gps_data_seabiRdsFIXED.RDS")

#seabirds deployment file
dep_dataPeru<-readRDS("data/dep_dataPeru_seabiRds.RDS")

#gps.data<-gps.data[which(gps.data$dep_id=="A21PEBO_19112019_A104"),] # select a  bird to check stuff
#gps.data<-gps.data[which(gps.data$time>"2019-11-18 16:57:06"),] # select a  bird to check stuff

#produce YEAR and SPECIES variables 
#filter out very short bursts of GPS fixes for both species
gps.data <- gps.data %>% 
  filter(!(dt <= (1/120)))

#clean data again with speeds less than 80km/h (with seabiRds)
gps.data <- cleanGPSData(data = as.data.frame(gps.data),
                               deployments = dep_dataPeru,
                                speedThreshold = 81, ##ACTIVATE for FINAL SUMMAGPS
                               plot = FALSE) #81 km/h treshold kills about 37 fast points

#produce YEAR and SPECIES variables 
#filter out very short bursts of GPS fixes for GUCOs (leaving the ones for PEBOs as are insignificant)

gps.data <- gps.data %>% 
  mutate(
    species = substring(dep_id, 4, 7),
    time = lubridate::with_tz(time,tz = "America/Lima"), #### ACTIVATE FOR FINAL SUMMAGPS
    year = lubridate::year(time),
    day = lubridate::day(time)) %>%  
    filter(!(dt <= (1/360) & species == "GUCO"))#filter out very short bursts of GPS fixes for GUCOs (leaving the ones for PEBOs as are insignificant)


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

#summarise trips without discarding specific trips, at 1km of colony
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



#clean out trips less than 10 GPS points MAY NEED TO INCREASE for RST
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
  filter(trip_id == min(trip_id)) #closest to sampling for GUCO

b<-SUMMAGPS %>%
  filter(Spec == "PEBO") %>%
  group_by(dep_id) %>% 
  filter(trip_id == max(trip_id)) #closest to sampling for PEBO

SUMMAGPS_1trip<-rbind(a,b)

#saveRDS(SUMMAGPS_1trip, file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1tripFULL.RDS") #only 1 trip per bird

xxx <- readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1tripFULL.RDS") #only 1 trip per bird


SUMMAGPS_1tripFULL #all trips including overnight
SUMMAGPS_1trip # excluding overnights

##############################################################################################################################
#gps.data<-gps.data[!is.na(gps.data$lat),]
