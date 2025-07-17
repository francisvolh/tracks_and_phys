library(trajr)
library(sf)
library(terra)

# Code to calculate Sinuosity and PCA of three highly correlated covariates 
#(Trip Duration, Max trip duration, and Total trip path)

#fixed trips
gps.data<-readRDS("data/gps.data_clean_INT.RDS") ## cleanned and ready???? testing

SUMMAGPS_clean<-readRDS("data/SUMMAGPS_clean.RDS")
#subset 1 bird

allbirds <- gps.data |> 
  dplyr::filter(unique_trip %in% (unique(SUMMAGPS_clean$unique_trip)))|>
  dplyr::filter(!is.na(y)) 



#Conversion of data frame to sf object

sins<-NULL

for (i in unique(allbirds$unique_trip)) {
  
  one_trip<-allbirds[which(allbirds$unique_trip == i),]
  
  sfUTM <- sf::st_as_sf(x = one_trip,                         
                    coords = c("x", "y"),
                    crs =  5387)
  
  
  #sfUTM <- sf::st_transform(df_sf, crs = "+proj=utm +zone=17")
  
  coord_onebird<-sf::st_coordinates(sfUTM)
  
  #ggplot2::ggplot()+
   # ggplot2::geom_point(data = coord_onebird, ggplot2::aes(x=X, y=Y))
  
  df_onebird<-as.data.frame(sfUTM)
  
  df_onebird<-cbind(df_onebird,coord_onebird)
  
  
  df_onebird<-df_onebird[,c("X", "Y","ID", "time")]
  
  trj <- trajr::TrajFromCoords(df_onebird)
  
  valSin<-trajr::TrajSinuosity2(trj)
  
  sins <- rbind(sins, valSin)
  
  #some sins are producing NaN
}

unique_trips <- unique(allbirds$unique_trip)

sins<-cbind(sins,unique_trips)

sins_trips<-as.data.frame(sins, row.names = FALSE)

sins_trips$V1<-as.numeric(sins_trips$V1)


hist(sins_trips$V1)

sins_trips[which(sins_trips$V1 == max(sins_trips$V1, na.rm= TRUE)),]
sins_trips[which(sins_trips$V1 == min(sins_trips$V1, na.rm= TRUE)),]

sins_trips[which(sins_trips$V1 >1),]


SUMMAGPS<-SUMMAGPS_clean


unique_tripsIDS<- unique(SUMMAGPS$unique_trip)

sins_trips <- sins_trips |>
  dplyr::filter(unique_trips %in% unique_tripsIDS)

glmm_tests<-merge(SUMMAGPS, sins_trips, by.x="unique_trip", by.y="unique_trips", all=TRUE)


glmm_tests <- glmm_tests |> 
  dplyr::rename(
    sinuos = V1
  )

names(glmm_tests)

head(glmm_tests)

#check relationship sinous and max dist

plot(glmm_tests$sinuos, glmm_tests$maxdist)

# check all, then remove long trips, in tiime first, and in distance after, and then both
ggpubr::ggscatter(glmm_tests#[which( glmm_tests$tottime <8 & glmm_tests$maxdist <60),]
                  , 
                  y = "sinuos", 
                  x = "maxdist",
                  color = "black", shape = 21, #size = 3, # Points color, shape and size
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                  cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                        label.sep = "\n"),
                  title = "effect of max dist on sinuosity calculation"
)

# NO effect!

#saveRDS(glmm_tests, "data/glmm_testsv2.RDS")



###PCA for metrics and model with covariate
names(glmm_tests)
res.pca <- prcomp(glmm_tests[, c("maxdist","tottime","totdist", "sinuos")], scale = TRUE) #, "sinuos" 

png("plots/PCA_fourvarsv2.png", height = 5, width = 5, units = "in" , res = 300)

factoextra::fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
dev.off()

groups <- as.factor(glmm_tests$Spec)
factoextra::fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

factoextra::fviz_pca_ind(res.pca,
             axes = c(1, 2),
             label = "none", # hide individual labels
             habillage = glmm_tests$Spec, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE # Concentration ellipses
)
res.pca <- prcomp(glmm_tests[, c("maxdist","tottime","totdist")], scale = TRUE) #, "sinuos" 

res.var <- factoextra::get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- factoextra::get_pca_ind(res.pca)
#res.ind$coord          # Coordinates
#res.ind$contrib        # Contributions to the PCs
#res.ind$cos2           # Quality of representation 


glmm_tests$PC1 <- res.ind$coord[,1]
glmm_tests$PC2 <- res.ind$coord[,2]
#saveRDS(glmm_tests, "data/glmm_testsPCAv2.RDS") # this version had totdist as meters, not kms, so may be diff PCA results (numerically, in the plots they look the same as new version) 
#also, confirm this is the on withOUT sinuosity included in PC1!!

#PCA of only totdist, tottime, and maxdist, sinousity remains raw and not on PC
#saveRDS(glmm_tests, "data/glmm_testsPCAv3.RDS") #v3 is produced on July 16th
                                                #after fixing issue with totdist in m to kms
                                

