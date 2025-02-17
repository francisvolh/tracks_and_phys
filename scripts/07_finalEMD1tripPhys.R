library(move)
#Create move object, run EMD (similarity) similarity analysis, cluster similar tracks, 
# plot and test differences

#have not tried this with the interpolated data
# reproducing code will need tweaks to used older summary metrics, non interpolated

#########################################
#########################################
### 1. Preload and prep move object (number of trips or individuals can substantially change the time of prcessing)
###    Only have run it with 37 birds and it takes 2.5 days
###    Run on DRA for 37 birds with pre-loaded move object
###
### @@@ SKIP to next step when loading already created move objects
#########################################
#########################################

#
#
#get all dep_id indistinct of metabolites, just from the final summry of 
#ONLY WITH 1 last trip (not mean trips)
#physiology and mean trip values
glmm_Test_2spPOST1<-b # comes from script 06_physefforstandPhysModelsv3


forEMDids <- glmm_Test_2spPOST1 |>
  #dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, unique_trip, steps) |>
  #filter(!is.na(glu)) |>
  dplyr::filter(steps>10)
unique(forEMDids$unique_trip)


#get GPS raw data fixed
gps.data<-readRDS("data/gps_data_seabiRdsFIXEDv2.RDS") #uses non interpolated data still
unique(gps.data$unique_trip)

emd.gps<-gps.data |>
  dplyr::filter(unique_trip %in% unique(forEMDids$unique_trip))

emd.gps<-emd.gps[order(emd.gps$dep_id, emd.gps$time),]

emd.gps <- as.data.frame(emd.gps)


#clean run for CC with all birds, 10 steps per trips (previous run had 30 and lost 4 birds/trips)
#moveobject<-readRDS("data/moveobjectALL.RDS") # all contains 56 trips, it didnt have enough time with 15days


#make move object
moveobjectALL <- move::move(x = emd.gps$lon , 
                      y = emd.gps$lat ,
                      time = as.POSIXct(emd.gps$time, format="%Y-%m-%d %H:%M:%OS", tz="America/Lima") ,
                      data = emd.gps,
                      proj = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") ,
                      animal = emd.gps$dep_id
)


##################################################################
##################################################################
######### 2. Work on moveobject (already saved)
#########
######### Two version of objects: 37 trips (move object 2) or 56 trips (moveobject_all) 
######### Run in the HCP clusters for 2 days with raw fixes and 37 trips, 
###################################################################

moveobject <- readRDS("data/moveobjectALL.RDS") # all 57 tracks

moveobject <- readRDS("data/moveobject2.RDS") # 37 tracks over 10 steps


moveobject<-spTransform(moveobject, center=T)
e<-extent(moveobject)+200000
ras<-raster(e, resolution=300, crs=crs(moveobject))
UDS <- list()
for (i in seq_len(nrow(idData(moveobject)))) {
  indiv<-moveobject[[i]]
  #trip_id<-paste(indiv$trip_id[-1], sep='_', timeLag(indiv, units='mins')<60)
  #indivB <- burst(x=indiv, f=trip_id)
  #indivB <- burst(indiv,indiv$unique_trip[-n.locs(indiv)])
  # not bursting because it is only 1 segment per individual
  dBBindiv <- brownian.bridge.dyn(indiv, raster=ras, location.error=5, margin= 5, window.size = 11
                                  # , burstType=grep(value=T, 'TRUE', unique(burstId(indivB)))
  ) 
  name00<-as.character(idData(indiv)[9])
  dBBindiv$layer@data@names<-name00
  UDS[[i]] <-dBBindiv
}
#plot(UDS[[2]]) #check UDs for each bursted individual
rasterStack<-stack(UDS)
sum(cellStats(rasterStack, sum))

system.time({
  emdDists <- emd(rasterStack / cellStats(rasterStack, sum))
})

saveRDS(emdDists, "emdDistsBCKP.RDS")
###################################################################################################
##################################################################################################

emdDists<-readRDS(file.choose())

library(ape)
oneclust<-hclust(emdDists, method = "ward.D2")
plot(oneclust)

tree <- as.phylo(hclust(emdDists)) 
abline(h=50000, col="red")
p<-plot(
  tree, 'fan', no.margin = T, x.lim = c(-0.65,0.65), cex = 0.7,
  # tip.color = c("red","blue")[grepl('day',tree$tip.label) + 1],
  label.offset = 0.01)

#saveRDS(p, "plotemdALL.RDS")

groups <- cutree(oneclust, h = 50000)



groups <- as.data.frame(groups)
groups$Trip_ID <- rownames(groups)
groups$species <- stringr::str_sub(groups$Trip_ID, 4,7)

# Extract the year if it is 2021
groups$year <- ifelse(grepl("2018", groups$Trip_ID),
                      "2018",
                      "2019")


table(groups$group, groups$species)
table(groups$year, groups$species)
table(groups$year, groups$group)


groups[which(groups$group == 4),]


rownames(groups) <- NULL
groups <- groups[order(groups$group), ]
head(groups)

groups$Trip_ID2<-gsub("._trip_.", " _trip_ ", groups$Trip_ID)
head(groups)
groups$Trip_ID2<-gsub("\\.", "-", groups$Trip_ID2)
head(groups)

##################################################################
##### Plot tracks for the 37 birds and the corresponding cluster
##################################################################

gps.data<-readRDS(file.choose())
emd.gps <- gps.data |>
  dplyr::filter(unique_trip %in% unique(groups$Trip_ID2))

emd.gps <- merge(emd.gps, groups, by.x="unique_trip", by.y="Trip_ID2", all=TRUE)

head(emd.gps)


emd.gps <- emd.gps |>
  dplyr::filter(!is.na(lon)) 

world <- sf::st_read("data/gadm36_PER_shp/gadm36_PER_0.shp")


#EMD Clusters map

p <-  ggplot2::ggplot() +
  ggplot2::geom_path(data=emd.gps, ggplot2::
                       aes(x = lon, y = lat, 
                              linetype=as.factor(species.x),
                              color = as.factor(groups)
  ), 
  cex= 0.5) +
  ggplot2::xlab("Longitude")+
  ggplot2::ylab("Latitude")+
  ggplot2::geom_sf(data = world, ggplot2::aes()) + #+ #add basemap
  ggplot2::coord_sf(crs = 4326, xlim = range(emd.gps$lon), ylim = range(emd.gps$lat)
  )+
  ggplot2::theme_bw()+
  ggplot2::labs(color = "EMD Cluster", linetype = "Species")
p
#+
#scale_shape_manual(values=c(0, 1, 5, 6))#+xlim = range(toplot$lon), ylim = range(toplot$lat)

#annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
#        color="black") +
# guides(color = guide_legend(override.aes = list(size = 4)) ) +
#theme(
# legend.position = c(0.99, 0.99),
#legend.justification = c(1,1),
#legend.text = element_text(size = 15)

#another way to change the titles
#p +  guides(color=guide_legend(title="EMD Cluster"))+
# guides(linetype="none")
ggplot2::ggsave("plots/EMDclusterTracks1v2.png", p, dpi = 300, bg = "white", units = 'in', width = 5, height = 7)


unique(emd.gps$unique_trip)



#attempting a regression with 

#make emd average object
reg_emd <- emdDists
class(reg_emd)
reg_emd <- as.matrix(reg_emd)
reg_emd <- as.data.frame(reg_emd)


rownames(reg_emd) <-1:nrow(reg_emd)

reg_emd <- reg_emd |>
  dplyr::mutate(replace(reg_emd, reg_emd == 0.00, "NA"))


reg_emd <- reg_emd |>
  tidyr::pivot_longer(1:ncol(reg_emd), names_to = "unique_trip", values_to = "emd_dist")

#clean the 2018 out of the average!!! 
clean_idx <- grepl("2018", reg_emd$unique_trip)

reg_emd <- reg_emd[-clean_idx,]

mean_emds <- reg_emd |>
  dplyr::select(unique_trip, emd_dist)|>
  dplyr::mutate(
    emd_dist = as.numeric(emd_dist)
  ) |>
  dplyr::group_by(unique_trip) |> 
  dplyr::summarise(
    meanEMD = mean(emd_dist, na.rm=TRUE)
  )

mean_emds<-as.data.frame(mean_emds)

mean_emds$unique_trip <- gsub("._trip_.", "_trip_", mean_emds$unique_trip)

mean_emds$unique_trip<-gsub("\\.", "-", mean_emds$unique_trip)
head(mean_emds)
View(mean_emds)

#physiology and mean trip values

#merge emd groups with phys data
glmm_Test_2spPOST1<-b

groups$Trip_ID2<-gsub("._trip_.", "_trip_", groups$Trip_ID2)

emd.phys <- merge(glmm_Test_2spPOST1, groups, by.x="unique_trip", by.y="Trip_ID2", all=TRUE)

emd.means_phys <- merge(glmm_Test_2spPOST1, mean_emds, by.x="unique_trip", by.y="unique_trip", all=TRUE)

head(emd.means_phys)

#glu reg
phystestsGLU <- emd.means_phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, meanEMD) |>
  dplyr::filter(!is.na(glu)) |>
  dplyr::filter(!is.na(meanEMD)) |>
  dplyr::filter(Year == 2019) |>
  dplyr::filter(Spec == "PEBO")

options(na.action = "na.omit")
reg0 <- lm(log(glu) ~ 1, data = phystestsGLU)
reg1 <- lm(log(glu) ~ log(meanEMD),  data = phystestsGLU)
options(na.action = "na.fail")
aic_lmGlu<-MuMIn::model.sel(reg0, reg1)
aic_lmGlu

A<-ggplot2::ggplot(phystestsGLU)+
  ggplot2::geom_point(ggplot2::aes(x=log(meanEMD), y = log(glu)))+
  ggplot2::geom_smooth(method="lm", aes(x=log(meanEMD), y = log(glu) ))

A

#chol reg
phystestsCHOL <- emd.means_phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, meanEMD) |>
  dplyr::filter(!is.na(chol)) |>
  dplyr::filter(!is.na(meanEMD))|>
  dplyr::filter(Year == 2019) |>
  dplyr::filter(Spec == "PEBO")

options(na.action = "na.omit")
reg0 <- lm(log(chol) ~ 1, data = phystestsCHOL)
reg1 <- lm(log(chol) ~ log(meanEMD),  data = phystestsCHOL)
options(na.action = "na.fail")
aic_lmChol<-MuMIn::model.sel(reg0, reg1)
aic_lmChol

B<-ggplot2::ggplot(phystestsCHOL)+
  ggplot2::geom_point(ggplot2::aes(x=log(meanEMD), y = log(chol)))+
  ggplot2::geom_smooth(method="lm", aes(x=log(meanEMD), y = log(chol) ))
B
#tri reg
phystestsTRI <- emd.means_phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, meanEMD) |>
  dplyr::filter(!is.na(tri)) |>
  dplyr::filter(!is.na(meanEMD))|>
  dplyr::filter(Year == 2019) |>
  dplyr::filter(Spec == "PEBO")

options(na.action = "na.omit")
reg0 <- lm(log(tri) ~ 1, data = phystestsTRI)
reg1 <- lm(log(tri) ~ log(meanEMD),  data = phystestsTRI)
options(na.action = "na.fail")
aic_lmTri<-MuMIn::model.sel(reg0, reg1)
aic_lmTri

C<- ggplot2::ggplot(phystestsTRI)+
  ggplot2::geom_point(ggplot2::aes(x=log(meanEMD), y = log(tri)))+
  ggplot2::geom_smooth(method="lm", aes(x=log(meanEMD), y = log(tri) ))
C



#ket reg
phystestsKET <- emd.means_phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, meanEMD) |>
  dplyr::filter(!is.na(ket)) |>
  dplyr::filter(!is.na(meanEMD))|>
  dplyr::filter(Year == 2019) |>
  dplyr::filter(Spec == "PEBO")

options(na.action = "na.omit")
reg0 <- lm(log(ket) ~ 1, data = phystestsKET)
reg1 <- lm(log(ket) ~ log(meanEMD),  data = phystestsKET)
options(na.action = "na.fail")
aic_lmKet<-MuMIn::model.sel(reg0, reg1)
aic_lmKet

D <- ggplot2::ggplot(phystestsKET)+
  ggplot2::geom_point(ggplot2::aes(x=log(meanEMD), y = log(ket)))+
  ggplot2::geom_smooth(method="lm", aes(x=log(meanEMD), y = log(ket) ))
D

metabsRegres<-cowplot::plot_grid(A, B, C, D, labels = c('A', 'B', 'C', 'D'), ncol = 2)


#missing DF for 1 group in these comparisons
##NEED TO FIX, some trips are missing in the EMD for some reason
#glu
phystestsGLUP1 <- emd.phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, groups) |>
  dplyr::filter(!is.na(glu)) |>
  dplyr::filter(!is.na(groups)) |>
  dplyr::filter(Spec == "PEBO")|>
  dplyr::filter(Year == 2019) |>
  dplyr::filter(groups !=4)

options(na.action = "na.omit")
reg0 <- lm(log(glu) ~ 1, data = phystestsGLUP1)
reg1 <- lm(log(glu) ~ as.factor(groups),  data = phystestsGLUP1)


options(na.action = "na.fail")
aic_lmGluP<-MuMIn::model.sel(reg0, reg1)

aic_lmGluP

A <- ggplot2::ggplot(phystestsGLUP1)+
  ggplot2::geom_boxplot(ggplot2::aes(x=as.factor(groups), y = log(glu), col = as.factor(groups)))+
  ggplot2::theme(legend.position = "none")+
  ggplot2::xlab(NULL)
A
#chol
phystestsCHOL1 <- emd.phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, groups) |>
  dplyr::filter(!is.na(chol)) |>
  dplyr::filter(!is.na(groups)) |>
  dplyr::filter(Spec == "PEBO")|>
  dplyr::filter(Year == 2019)|>
  dplyr::filter(groups !=4)

options(na.action = "na.omit")
reg0 <- lm(log(chol) ~ 1, data = phystestsCHOL1)
reg1 <- lm(log(chol) ~ as.factor(groups),  data = phystestsCHOL1)


options(na.action = "na.fail")
aic_lmCholP<-MuMIn::model.sel(reg0, reg1)

aic_lmCholP

B <- ggplot(phystestsCHOL1)+
  geom_boxplot(aes(x=as.factor(groups), y = log(chol), col = as.factor(groups)))+
  theme(legend.position = "none")+
  xlab(NULL)
B
#tri
phystestsTRI1 <- emd.phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, groups) |>
  dplyr::filter(!is.na(tri)) |>
  dplyr::dplyr::filter(!is.na(groups)) |>
  dplyr::filter(Spec == "PEBO")|>
  dplyr::filter(groups !=4)

options(na.action = "na.omit")
reg0 <- lm(log(tri) ~ 1, data = phystestsTRI1)
reg1 <- lm(log(tri) ~ as.factor(groups),  data = phystestsTRI1)


options(na.action = "na.fail")
aic_lmTriP<-MuMIn::model.sel(reg0, reg1)

aic_lmTriP

C <- ggplot(phystestsTRI1)+
  geom_boxplot(aes(x=as.factor(groups), y = log(tri), col = as.factor(groups)))+
  theme(legend.position = "none")+
  xlab("EMD Cluster")
C

#tri
phystestsKET1 <- emd.phys |>
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id, groups) |>
  dplyr::filter(!is.na(ket)) |>
  dplyr::filter(!is.na(groups)) |>
  dplyr::filter(Spec == "PEBO") |>
  dplyr::filter(groups !=4)#|>
#dplyr::mutate(
 # groups = as.factor(groups),
  #ket = log(ket)
#)

table(phystestsKET1$groups)

options(na.action = "na.omit")
reg0 <- lm(log(ket) ~ 1, data = phystestsKET1)
reg1 <- lm(log(ket) ~ as.factor(groups),  data = phystestsKET1)

reg1i <- aov(log(ket) ~ as.factor(groups),  data = phystestsKET1)

TukeyHSD(reg1i, conf.level=.95)

options(na.action = "na.fail")
aic_lmTriP<-MuMIn::model.sel(reg0, reg1)

aic_lmTriP
summary(reg1)

D <- ggplot2::ggplot(phystestsKET1)+
  ggplot2::geom_boxplot(ggplot2::aes(x=as.factor(groups), y = log(ket), col = as.factor(groups)))+
  ggplot2::theme(legend.position = "none")+
  ggplot2::xlab("EMD Cluster")
D
metabsAnova <- cowplot::plot_grid(A, B, C, D, labels = c('A', 'B', 'C', 'D'), ncol = 2)
metabsAnova

library(ggsignif)
ggplot2::ggplot(data= phystestsKET1, ggplot2::aes(x=groups, y = log(ket), col= groups))+
  geom_boxplot()+
  #theme(legend.position = "none")+
  #xlab("EMD Cluster") +
  geom_signif(
    comparisons = list(c("1", "2"),
                       c("1", "3"),
                       c("1", "4"), 
                       c("2", "3"),
                       c("2", "4"),
                       c("3", "4")
    ),
    y_position = c(0.5, 0.8, 1.1, 1.4, 1.7, 2), 
    #xmin = c(0.8, 1.8), 
    #xmax = c(1.2, 2.2),
    #annotation = c("**", "NS"), 
    tip_length = 0,
    map_signif_level = TRUE, 
    test = "TukeyHSD"
  )#+
geom_signif(
  comparisons = list(c("3", "1"))
)

class(phystestsKET1$groups)
