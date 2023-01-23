devtools::install_github("allisonglider/seabiRds@dev", 
                         build_vignettes = T, 
                         upgrade = 'never', force = TRUE)
library(seabiRds)

#vignette(topic = 'Ecotone-GPS-Example', package = 'seabiRds')
#vignette(topic = 'Technosmart-GPS-Example', package = 'seabiRds')

####Ecotone
ecofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/ECOTONES/master"

setwd(ecofolder)

raw_dep <- read.csv("C:/Users/francis van oordtis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deployments2019new.csv", 
                    stringsAsFactors = F)

idx <- grep("COR", raw_dep$unit)

raw_dep <- raw_dep[idx,]

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)

raw_dep$Date_Recap<-sub("remote", NA, raw_dep$Date_Recap)

raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$rec_time)

raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$dp_ID<-paste0(raw_dep$colour_band, "_", raw_dep$unit) #only for ECOTONE DATA
#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"GNNE"

raw_dep$status<-"C"
#write.csv(raw_dep, file="raw_dep.csv")

dep_dataECO<-formatDeployments(deployments = raw_dep,
                            dateFormat = "%m/%d/%Y %H:%M",
                            dep_tz ="America/Lima",
                            species = 'SP',
                            metal_band = 'metal_band',
                            colour_band = 'colour_band',
                            dep_id = 'dp_ID',
                            fill_dep_id = T,
                            site = 'site',
                            subsite = "subsite",
                            dep_lon = 'nest_long',
                            dep_lat = 'nest_lat',
                            time_released = 'timeon',
                            time_recaptured = 'timeoff',
                            status_on = 'status',
                            #status_off = 'status',
                            mass_on = 'mass',
                            #mass_off = 'Mass_off',
                            gps_id = 'unit',#
                            #tdr_id = 'TDR',
                            exclude = 'exclude'
)

dep_dataECO$tdr_id[dep_dataECO$dep_id=="G13GUCO_COR08_2019"]<-dep_dataECO$gps_id[dep_dataECO$dep_id=="G13GUCO_COR08_2019"]

fulldataEco<-readGPSData(ecofolder, dep_dataECO, tagType = "Ecotone")

ecotone_data <- cleanGPSData(data = fulldataEco,
                             deployments = dep_dataECO,
                             speedThreshold = 150,
                             plot = TRUE)



saveRDS(dep_dataECO,file ="dep_dataECO.RDS")
saveRDS(ecotone_data, file ="ecotone_final.RDS")

#################################################
#######Technosmart
technofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps"

setwd(technofolder)

raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deploymentsAxxys2019.csv", stringsAsFactors = F)
#raw_dep <- raw_dep[-22,]

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)

#raw_dep$Date_Recap<-sub("remote", NA, raw_dep$Date_Recap)

raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$Time_Recap)

raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"

dep_dataDLW<-formatDeployments(deployments = raw_dep,
                            dateFormat = "%d-%b-%y  %H:%M",
                            dep_tz ="America/Lima",
                            species = 'SP',
                            metal_band = 'metal_band',
                            colour_band = 'colour_band',
                            dep_id = 'dp_ID',
                            fill_dep_id = T,
                            site = 'site',
                            subsite = "subsite",
                            dep_lon = 'nest_long',
                            dep_lat = 'nest_lat',
                            time_released = 'timeon',
                            time_recaptured = 'timeoff',
                            status_on = 'status',
                            #status_off = 'status',
                            mass_on = 'mass',
                            #mass_off = 'Mass_off',
                            gps_id = 'unit',
                            tdr_id = 'unit',
                            acc_id = 'unit'
)

technoDLW_data<-readGPSData(technofolder, dep_dataDLW, tagType = "Technosmart")

technoDLW_datai <- cleanGPSData(data = technoDLW_data,
                             deployments = dep_dataDLW,
                             speedThreshold = 150,
                             plot = FALSE)

saveRDS(dep_dataDLW,file ="dep_dataDLW.RDS")
saveRDS(technoDLW_datai, file ="technoDLW_datai.RDS")

#######igotu 2019
igotufolder2019<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/BACKUPTRACKS2019"

setwd(igotufolder2019)

raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deployments2019new.csv", stringsAsFactors = F, na.strings=c("","NA"))
raw_dep <- raw_dep[-29,]
idx <- grep("COR", raw_dep$unit)

raw_dep <- raw_dep[-idx,]

idx <- grep("A", raw_dep$unit)

raw_dep <- raw_dep[-idx,]

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)


raw_dep$timeoff<-NA
raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$rec_time)
raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"

dep_dataigotu2019<-formatDeployments(deployments = raw_dep,
                               dateFormat = "%m/%d/%Y %H:%M",
                               dep_tz ="America/Lima",
                               species = 'SP',
                               metal_band = 'metal_band',
                               colour_band = 'colour_band',
                               dep_id = 'dp_ID',
                               fill_dep_id = T,
                               site = 'site',
                               subsite = "subsite",
                               dep_lon = 'nest_long',
                               dep_lat = 'nest_lat',
                               time_released = 'timeon',
                               time_recaptured = 'timeoff',
                               status_on = 'status',
                               #status_off = 'status',
                               mass_on = 'mass',
                               #mass_off = 'Mass_off',
                               gps_id = 'unit'#,
                               #tdr_id = 'TDR'
)

fulldataigot2019<-readGPSData(igotufolder2019, dep_dataigotu2019, dateFormat ='%Y/%m/%d', tagTZ="America/Lima",  tagType = "Cattrack")

igotu2019_data <- cleanGPSData(data = fulldataigot2019,
                                deployments = dep_dataigotu2019,
                                speedThreshold = 150,
                                plot = FALSE)

saveRDS(dep_dataigotu2019,file ="dep_dataigotu2019.RDS")
#
saveRDS(igotu2019_data, file ="igotu2019_data.RDS")


#######Technosmart NO DLW
technofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps"

setwd(technofolder)

raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deployments2019new.csv", stringsAsFactors = F, na.strings=c("","NA"))
raw_dep <- raw_dep[-29,]
raw_dep <- raw_dep[-(1:16),]

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)

raw_dep$Date_Recap<-sub("remote", NA, raw_dep$Date_Recap)

raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$rec_time)

raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"
#raw_dep <-raw_dep[complete.cases(raw_dep[c("dp_ID")]),]
raw_dep$TDR<-raw_dep$unit

dep_datanoDLW<-formatDeployments(deployments = raw_dep,
                               dateFormat = "%m/%d/%Y  %H:%M",
                               dep_tz ="America/Lima",
                               species = 'SP',
                               metal_band = 'metal_band',
                               colour_band = 'colour_band',
                               dep_id = 'dp_ID',
                               fill_dep_id = F,
                               site = 'site',
                               subsite = "subsite",
                               dep_lon = 'nest_long',
                               dep_lat = 'nest_lat',
                               time_released = 'timeon',
                               time_recaptured = 'timeoff',
                               status_on = 'status',
                               #status_off = 'status',
                               mass_on = 'mass',
                               #mass_off = 'Mass_off',
                               gps_id = 'unit',
                               tdr_id = 'TDR',
                               acc_id = 'unit'
)

technonoDLW_data<-readGPSData(technofolder, dep_datanoDLW, tagType = "Technosmart")

technonoDLW_datai <- cleanGPSData(data = technonoDLW_data,
                                deployments = dep_datanoDLW,
                                speedThreshold = 150,
                                plot = TRUE)


saveRDS(dep_datanoDLW,file ="dep_datanoDLW.RDS")
saveRDS(technonoDLW_datai, file ="technonoDLW_datai.RDS")


#######igotu 2018
igotufolder2018<-"C:/Users/francis van oordtis van oordt/OneDrive - McGill University/Documents/McGill/Field data/02 GN 2018/BACKUP TRACKS"

setwd(igotufolder2018)

raw_dep <- read.csv("C:/Users/francis van oordtis van oordt/OneDrive - McGill University/Documents/McGill/Field data/deployments2018.csv", 
                    stringsAsFactors = FALSE, na.strings=c("","NA"))

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)


raw_dep$timeoff<-NA
raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$rec_time)

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"
levels(raw_dep$timeoff)[levels(raw_dep$timeoff)=="NA NA"] <- NA
raw_dep$timeoff[raw_dep$timeoff == 'NA NA'] <-  NA

#idx <- grep("NA NA", raw_dep$timeoff)
#raw_dep <- raw_dep[-idx,] #dont subset if including all deployments, 
#even not recovered ones
#idx <- grep("WATERLOGGED", raw_dep$Observations)
#raw_dep <- raw_dep[-idx,]

dep_dataigotu2018<-formatDeployments(deployments = raw_dep,
                                     dateFormat = "%m/%d/%Y %H:%M",
                                     dep_tz ="America/Lima",
                                     species = 'SP',
                                     metal_band = 'metal_band',
                                     colour_band = 'colour_band',
                                     dep_id = 'dp_ID',
                                     fill_dep_id = T,
                                     site = 'site',
                                     subsite = "subsite",
                                     dep_lon = 'nest_long',
                                     dep_lat = 'nest_lat',
                                     time_released = 'timeon',
                                     time_recaptured = 'timeoff',
                                     status_on = 'status',
                                     #status_off = 'status',
                                     mass_on = 'mass',
                                     #mass_off = 'Mass_off',
                                     gps_id = 'unit'#,
                                     #tdr_id = 'TDR'
)

fulldataigot2018<-readGPSData(igotufolder2018, dep_dataigotu2018, dateFormat ='%Y/%m/%d', tagTZ="America/Lima",  tagType = "Cattrack")

igotu2018_data <- cleanGPSData(data = fulldataigot2018,
                               deployments = dep_dataigotu2018,
                               speedThreshold = 150,
                               plot = FALSE)

saveRDS(dep_dataigotu2018,file ="dep_dataigotu2018.RDS")
#
saveRDS(igotu2018_data, file ="igotu2018_data.RDS")

####################
####################
####################
#CAMs 
camGPSfolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/cam ARDs/gps"
setwd(camGPSfolder)

raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deploys2019cameras.csv", na.strings=c("","NA"))

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)

#raw_dep$Date_Recap<-sub("remote", NA, raw_dep$Date_Recap)

raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$rec_time)

raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"
#raw_dep <-raw_dep[complete.cases(raw_dep[c("dp_ID")]),]

dep_dataCams<-formatDeployments(deployments = raw_dep,
                               dateFormat = "%m/%d/%Y %H:%M",
                               dep_tz ="America/Lima",
                               species = 'SP',
                               metal_band = 'metal_band',
                               colour_band = 'colour_band',
                               dep_id = 'dp_ID',
                               fill_dep_id = F,
                               site = 'site',
                               subsite = "subsite",
                               dep_lon = 'nest_long',
                               dep_lat = 'nest_lat',
                               time_released = 'timeon',
                               time_recaptured = 'timeoff',
                               status_on = 'status',
                               #status_off = 'status',
                               mass_on = 'mass',
                               #mass_off = 'Mass_off',
                               gps_id = 'unit',
                               tdr_id = 'unit',
                               acc_id = 'unit',
                               cam_id = 'cam_id'
)

technoCams<-readGPSData(camGPSfolder, dep_dataCams, tagType = "Technosmart")

technoCams_data <- cleanGPSData(data = technoCams,
                                deployments = dep_dataCams,
                                speedThreshold = 150,
                                plot = TRUE)


saveRDS(dep_dataCams,file ="dep_dataCams.RDS")
#
saveRDS(technoCams_data, file ="technoCams_data.RDS")

####################
####################
####################
#Load all RDS to merge into one dataframe

igotufolder2018<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/02 GN 2018/BACKUP TRACKS"
setwd(igotufolder2018)
igotu2018_data<-readRDS("igotu2018_data.RDS")
dep_dataigotu2018<-readRDS("dep_dataigotu2018.RDS")

igotufolder2019<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/BACKUPTRACKS2019"
setwd(igotufolder2019)
igotu2019_data<-readRDS("igotu2019_data.RDS")
dep_dataigotu2019<-readRDS("dep_dataigotu2019.RDS")


technofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps"
setwd(technofolder)
technonoDLW_datai<-readRDS("technonoDLW_datai.RDS")
dep_dataDLW<-readRDS("dep_dataDLW.RDS")

technofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps"
setwd(technofolder)
dep_datanoDLW<-readRDS("dep_datanoDLW.RDS")




technofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps"
setwd(technofolder)
technoDLW_datai<-readRDS("technoDLW_datai.RDS")

ecofolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/ECOTONES/master"
setwd(ecofolder)
ecotone_final<-readRDS("ecotone_final.RDS")
dep_dataECO<-readRDS("dep_dataECO.RDS")

camGPSfolder<-"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/cam ARDs/gps"
setwd(camGPSfolder)
technoCams_data<-readRDS(file ="technoCams_data.RDS")
dep_dataCams<-readRDS(file ="dep_dataCams.RDS")

#Deployment data

#bind dep_data of all devices
dep_dataPeru <- rbind(dep_dataigotu2018, 
                      dep_dataDLW, 
                      dep_datanoDLW, 
                      dep_dataCams, 
                      dep_dataigotu2019, 
                      dep_dataECO)
saveRDS(dep_dataPeru,file="C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS") 


dep_dataPeru<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")


####
for_database<-unique(dep_dataPeru[,c("site", "subsite", "dep_lon", "dep_lat")])

sum_subites<-for_database %>%
  group_by(site, subsite) %>% 
  summarise(
    meanLocLat = mean(dep_lat),
    meanLocLon = mean(dep_lon)
  )

saveRDS(sum_subites, file = 'C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/sum_subsites.RDS')
 

 #fix labels for Ecotones## OR MODIFY DEPLOYMENT SHEET CORRECTLY
  #ecotone_final$fixlab<-substr(ecotone_final$dep_id, start = 11, stop = 12)
  #ecotone_final$dep_id<-paste(ecotone_final$fixlab,"GUCO",year(as.POSIXct(ecotone_final$time)), sep = "")
  #unique(ecotone_final$dep_id)
  #ecotone_final$fixlab<-NULL


#join all dfs from aws pre files

#to create a label, NOT FOR AWS Server!!!
#aware of ecotone file to load or produce
gps.dataNEW <- rbind(igotu2018_data, technonoDLW_datai, igotu2019_data, technoDLW_datai, ecotone_final,technoCams_data)#be aware: ecotone_final or ecotone_data

#gps.data$toSPPlab3<-ifelse(grepl("PEBO", gps.data$dep_id), "PEBO", "GUCO")

#gps.data$BirdLabel<-NA
#gps.data$BirdLabel<-paste(substr(gps.data$dep_id, start = 1, stop = 7),year(as.POSIXct(gps.data$time)), sep = "")

#unique(gps.data$BirdLabel)

setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data")
saveRDS(gps.dataNEW,file="gps_data_seabiRds.RDS")
#last save JAN-10-2023, after detecting a deployment time error on PEBO C06

gps.data<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRds.RDS")
####################
####################


####################
#Techno for TDR axxy DLW only dep file
technofolder<-"E:/2019 GN/ARDs"
setwd(technofolder)

raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deploymentsAxxys2019.csv", stringsAsFactors = F)
#raw_dep <- raw_dep[-22,]

raw_dep$colour_band<-raw_dep$metal_band

raw_dep$metal_band<-NA

raw_dep$timeon<-paste(raw_dep$Date.Capt,raw_dep$timedploy)

#raw_dep$Date_Recap<-sub("remote", NA, raw_dep$Date_Recap)

raw_dep$timeoff<-paste(raw_dep$Date_Recap,raw_dep$Time_Recap)

raw_dep$timeoff[is.na(raw_dep$Date_Recap)]<-NA

#raw_dep$LOC<-"GuanapeN"
#raw_dep$subsite<-"house"

raw_dep$status<-"C"

dep_dataTDRdlw<-formatDeployments(deployments = raw_dep,
                               dateFormat = "%d-%b-%y  %H:%M",
                               dep_tz ="America/Lima",
                               species = 'SP',
                               metal_band = 'metal_band',
                               colour_band = 'colour_band',
                               dep_id = 'dp_ID',
                               fill_dep_id = T,
                               site = 'site',
                               subsite = "subsite",
                               dep_lon = 'nest_long',
                               dep_lat = 'nest_lat',
                               time_released = 'timeon',
                               time_recaptured = 'timeoff',
                               status_on = 'status',
                               #status_off = 'status',
                               mass_on = 'mass',
                               #mass_off = 'Mass_off',
                               gps_id = 'unit',
                               tdr_id = 'unit'
)

unique(dep_dataTDRdlw$dep_id)

#subset to GUCO only
#dep_dataTDRdlw<-dep_dataTDRdlw[dep_dataTDRdlw$species=="GUCO",]
#
#subset to certain individuals only
#dep_dataTDRdlw<-dep_dataTDRdlw[dep_dataTDRdlw$dep_id=="A06_PEBO_17112019_A105",]

technoTDRdlw_data <- readTDRData(
  inputFolder = technofolder,
  deployments = dep_dataTDRdlw,
  tagTZ = "UTC",
  tagType = 'Technosmart',
  dateFormat = "%d/%m/%Y" # might need to change this to match your data
)

technoTDRdlw_data<-cleanTDRData(technoTDRdlw_data, dep_dataTDRdlw, plot = T)
#A01<-technoTDRdlw_data
#A06<-technoTDRdlw_data

#bind in case you had broken reads
#technoTDRPeru_data_NEW<-rbind(technoTDRPeru_data_NEW, technoTDRdlw_data)
 

#AXXYs non DLW
technoTDRNOdlw_data <- readTDRData(
  inputFolder = technofolder,
  deployments = dep_datanoDLW,
  tagTZ = "UTC",
  tagType = 'Technosmart',
  dateFormat = "%d/%m/%Y" # might need to change this to match your data
)

technoTDRNOdlw_data <-cleanTDRData(technoTDRNOdlw_data , dep_datanoDLW, plot = T)
#
saveRDS(technoTDRNOdlw_data, file ="technoTDRNOdlw_data.RDS")
#read DLW tdr data 
#a<-readRDS("technoTDRdlw_data.RDS")

#bind both DLW and non-DLW deployments
technoTDRPeru_data<-rbind(technoTDRNOdlw_data, a)

#
saveRDS(technoTDRPeru_data, file ="technoTDRPeru_data.RDS")
depthdata<-readRDS("technoTDRPeru_data.RDS")
#saveRDS(dep_dataTDRdlw, file = "dep_dataTDRdlw.RDS")

#tdr data from ecotone

test_ecoTDRdata <- readTDRData(
  inputFolder = ecofolder,
  deployments = dep_dataECO,
  tagTZ = "UTC",
  tagType = 'Ecotone',
  dateFormat = "%d/%m/%Y" # might need to change this to match your data
)

test_eco <-cleanTDRData( test_eco, dep_dataECO, plot = T)

technoTDRPeru_data<-rbind(technoTDRPeru_data, test_eco)
#saveRDS(technoTDRPeru_data, file ="C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/technoTDRPeru_data_NEW.RDS")



##
#CAMs data
inputFolder<-"E:/2019 GN/cam ARDs"
setwd(inputFolder)
camTDRdata <- readTDRData(
  inputFolder = inputFolder,
  deployments = dep_dataCams,
  tagTZ = "UTC",
  tagType = 'Technosmart',
  dateFormat = "%d/%m/%Y" # might need to change this to match your data
)

technoTDRCAMsdata <-cleanTDRData(camTDRdata, dep_dataCams, plot = T)
#
saveRDS(technoTDRCAMsdata, file ="technoCAMs_data.RDS")


technoTDRPeru_data<-rbind(technoTDRPeru_data_NEW.RDS, technoTDRCAMsdata)
setwd("E:/2019 GN/ARDs")
technoTDRPeru_data_NEW<- readRDS("technoTDRPeru_data_NEW.RDS")
saveRDS(technoTDRPeru_data_NEW, file ="technoTDRPeru_data_NEW.RDS")

tdrdata<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/technoTDRPeru_data_NEW.RDS")

###################################################################################
###################################################################################
#try loop to read and write files NO NEED AS WE FOUND THE ISSUE WITH THE X MANAGER
a<-list.files(pattern="*.csv")

for (i in 1:length(a)) {
  b<-read.delim(file = a[i])
  write.csv(b, file =a[i], row.names = FALSE)
  print(a[i])
}
b<-read.csv("A03PEBO_A109_20191113_S1.csv", sep=";", nrows=10)
