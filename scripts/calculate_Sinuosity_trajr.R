library(tidyverse)
library(trajr)
library(sf)
library(terra)

#fixed trips
gps.data<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#subset 1 bird

allbirds <- gps.data %>% 
  filter(!is.na(lat)) 



#Conversion of data frame to sf object

sins<-NULL

for (i in unique(allbirds$unique_trip)) {
  one_trip<-allbirds[which(allbirds$unique_trip == i),]
  
  df_sf <- st_as_sf(x = one_trip,                         
                    coords = c("lon", "lat"),
                    crs = "+proj=longlat +datum=WGS84")
  
  
  sfUTM <- st_transform(df_sf, crs = "+proj=utm +zone=17")
  
  coord_onebird<-st_coordinates(sfUTM)
  
  df_onebird<-as.data.frame(sfUTM)
  
  df_onebird<-cbind(df_onebird,coord_onebird)
  
  
  df_onebird<-df_onebird[,c("X", "Y","dep_id", "time")]
  
  trj <- TrajFromCoords(df_onebird)
  
  valSin<-TrajSinuosity2(trj)
  
  sins <- rbind(sins, valSin)
}

unique_trips <- unique(allbirds$unique_trip)

sins<-cbind(sins,unique_trips)

sins_trips<-as.data.frame(sins, row.names = FALSE)

sins_trips$V1<-as.numeric(sins_trips$V1)
nastrips <- grep("NA",sins_trips$unique_trips)


sins_trips<-sins_trips[-nastrips,]
hist(sins_trips$V1)

sins_trips[which(sins_trips$V1 == max(sins_trips$V1, na.rm= TRUE)),]
sins_trips[which(sins_trips$V1 == min(sins_trips$V1, na.rm= TRUE)),]

#SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")


unique_tripsIDS<- unique(SUMMAGPS$unique_trip)

sins_trips <- sins_trips %>%
  filter(unique_trips %in% unique_tripsIDS)


hist(sins_trips$V1)

glmm_tests<-merge(SUMMAGPS, sins_trips, by.x="unique_trip", by.y="unique_trips", all=TRUE)
glmm_tests <- glmm_tests %>% 
  rename(
    sinuos = V1
  )
names(glmm_tests)

saveRDS(glmm_tests, "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_tests.RDS")
