#add points to trip that is missing data at the end
#specially for 1-trip models
library(dplyr)

######################ATTEMP to model gaps in ending trips

#look at trips with gaps
SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")
SUMMAGPS_1trip<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1trip.csv")

#get the data
gps.data<-readRDS(file="C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")



#fixed!!!!!!!!!!!!!!!
gps.data<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")



#detect the trips
SUMMAGPS %>% 
  filter(distCOLend > 10) %>% 
 # select("dep_id" , "trip_id", "distCOLend", "unique_trip") %>% 
  View()

#two trips from ONE TRIP summary
#A21PEBO_19112019_A104 _trip_ 5 DONE
#G01PEBO_31_20181125-152024 _trip_ 7 DONE

#G01PEBO_31_20181125-152024 _trip_ 5
#G08PEBO_33_20181127-150745 _trip_ 2

#G09GUCO_COR04_2019 _trip_ 15
#G09GUCO_COR04_2019 _trip_ 17
#G09GUCO_COR04_2019 _trip_ 20
#G09GUCO_COR04_2019 _trip_ 22
#G09GUCO_COR04_2019 _trip_ 23

#check from all trip summary
ccc<-SUMMAGPS %>% 
  filter(distCOLend > 10) %>% #greater than half and hour DT
  select("dep_id" , "trip_id", "maxDiffTime", "distCOLend", "unique_trip") # %>% 
  View()
#G09GUCO_COR04_20193
#G01PEBO_31_20181125-152024
#G08PEBO_33_20181127-150745 

#deployments
vetorDEPS<-unique(ccc$dep_id)
for (i in vetorDEPS) {
  toplot<-gps.data[which(gps.data$dep_id==i),]
  
  plot(toplot$time, toplot$coldist, type = "p", pch = 16, cex=0.3) 
  title(i)
  readline('next')
}
  #unique trips
vetorDEPS<-unique(ccc$unique_trip)
for (i in vetorDEPS) {
  toplot<-gps.data[which(gps.data$unique_trip==i),]
  
  plot(toplot$time, toplot$coldist, type = "p", pch = 16, cex=0.3) 
  title(i)

  readline('next')
}


#extract first point of trip to modify time according to last point of trip
# from mapping subset "toplot"
i <-  "G09GUCO_COR04_2019 _trip_ 23" # if want to do one specific run inside the loop only

#get trip ids of cut trips at the end
cutEnd<-unique(SUMMAGPS[which(SUMMAGPS$distCOLend >10),]$unique_trip)

#get DF with the starting POSITION of the trip (min time)
A<-NULL
for (i in cutEnd) {
 
   toplot<-gps.data[which(gps.data$unique_trip==i),]

    a<-toplot[which(toplot$time == min(toplot$time, na.rm=TRUE)),]

    A<-rbind(A,a)
}

#B for second point



#write csv of points to modify TIME outside in spreadsheet
write.csv(A, file = "addpointENDTRIPS.csv")



#now get the ending position of the trip to get calculate TIME 
#IN LOCAL TIME 
#of last leg to change in the DF that has the starting position min time  
#unique trips

C<-NULL
for (i in unique(SUMMAGPS[which(SUMMAGPS$distCOLend >10),]$unique_trip)) {
  
  toplot<-gps.data[which(gps.data$unique_trip==i),]
  
  a<-toplot[which(toplot$time == max(toplot$time, na.rm=TRUE)),]
  
  C<-rbind(C,a)
}

head(gps.data[which(gps.data$dep_id == "G08PEBO_33_20181127-150745" & gps.data$time > "2018-11-24 18:39:00"),],2)


##get first, last, and next point of those trips that end abruptly
D<-NULL
for (i in unique(SUMMAGPS[which(SUMMAGPS$distCOLend >10),]$unique_trip)) {
  
  id<-unique(gps.data[which(gps.data$unique_trip==i),]$dep_id)
  
  trip<-gps.data[which(gps.data$unique_trip == i),]
  
  ti1 <- min(trip$time, na.rm=TRUE)
  ti2 <- max(trip$time, na.rm=TRUE)
 
  a<-gps.data[which(gps.data$dep_id == id & gps.data$time == ti1),]
  
  b<-head(gps.data[which(gps.data$dep_id == id & gps.data$time >= ti2),],2)
  
  
  D<-rbind(D,a,b)
}

View(D)


### FIX FIX FIX
#CAREFULY CHOOSE POSITIONS, some may be to FAST!!!!!!!!!!!!!!!
#G08PEBO_33_20181127-150745 _trip_ 2
#G01PEBO_31_20181125-152024 _trip_ 5


#write.csv(C,"C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/addpointsD.csv")





##ADDING POINT
#after modifying, read A with new points
#A<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/addpointA.csv")
#B<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/addpointB.csv")
#A<-rbind(A,B)

#points in UTM
A<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/addpointENDFINAL.csv")
#correct posixct as it always get ruined by Excel
A$time<-as.POSIXct(A$time, format="%m/%d/%Y %H:%M",  tz="UTC")#, tz="America/Lima"


#A <- A %>% 
 # mutate(
   # time = lubridate::with_tz(time,tz = "UTC")
  #)

#bind gps raw original data with modified points, and re-run "seabiRds"
gps.data<-readRDS(file="C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")


dep_dataPeru<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")

gps.data<-rbind(gps.data,A)

gps.data<-gps.data[order(gps.data$dep_id, gps.data$time),]


gps.data <- cleanGPSData(data = gps.data,
                         deployments = dep_dataPeru,
                         speedThreshold = 110,
                         plot = FALSE)

#saveRDS(gps.data,"C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#View(head(gps.data[which(gps.data$dep_id == D$dep_id[4] & gps.data$time >= D$time[4]),],3))
