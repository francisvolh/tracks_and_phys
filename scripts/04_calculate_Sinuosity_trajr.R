library(trajr)
library(sf)
library(terra)

# Code to calculate Sinuosity and PCA of three highly correlated covariates 
#(Trip Duration, Max trip duration, and Total trip path)

#fixed trips
gps.data<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#subset 1 bird

allbirds <- gps.data |> 
  dplyr::filter(!is.na(lat)) 



#Conversion of data frame to sf object

sins<-NULL

for (i in unique(allbirds$unique_trip)) {
  one_trip<-allbirds[which(allbirds$unique_trip == i),]
  
  df_sf <- sf::st_as_sf(x = one_trip,                         
                    coords = c("lon", "lat"),
                    crs = "+proj=longlat +datum=WGS84")
  
  
  sfUTM <- sf::st_transform(df_sf, crs = "+proj=utm +zone=17")
  
  coord_onebird<-sf::st_coordinates(sfUTM)
  
  df_onebird<-as.data.frame(sfUTM)
  
  df_onebird<-cbind(df_onebird,coord_onebird)
  
  
  df_onebird<-df_onebird[,c("X", "Y","dep_id", "time")]
  
  trj <- trajr::TrajFromCoords(df_onebird)
  
  valSin<-trajr::TrajSinuosity2(trj)
  
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

SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")


unique_tripsIDS<- unique(SUMMAGPS$unique_trip)

sins_trips <- sins_trips |>
  dplyr::filter(unique_trips %in% unique_tripsIDS)


hist(sins_trips$V1)

glmm_tests<-merge(SUMMAGPS, sins_trips, by.x="unique_trip", by.y="unique_trips", all=TRUE)
glmm_tests <- glmm_tests |> 
  dplyr::rename(
    sinuos = V1
  )
names(glmm_tests)

#saveRDS(glmm_tests, "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_tests.RDS")



###PCA for metrics and model with covariate
names(glmm_tests)
res.pca <- prcomp(glmm_tests[, c("maxdist","TimeTrip","totdist")], scale = TRUE) #, "sinuos" 

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

groups <- as.factor(glmm_tests$Spec)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(res.pca,
             axes = c(1, 2),
             label = "none", # hide individual labels
             habillage = glmm_tests$Spec, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE # Concentration ellipses
)

res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- get_pca_ind(res.pca)
#res.ind$coord          # Coordinates
#res.ind$contrib        # Contributions to the PCs
#res.ind$cos2           # Quality of representation 


glmm_tests$PC1 <- res.ind$coord[,1]
glmm_tests$PC2 <- res.ind$coord[,2]

#saveRDS(glmm_tests, "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_testsPCA.RDS")


