library(move)

moveobject2<-readRDS("moveobject2.RDS")


moveobject2<-spTransform(moveobject2, center=T)
e<-extent(moveobject2)+200000
ras<-raster(e, resolution=300, crs=crs(moveobject2))
UDS <- list()
for (i in seq_len(nrow(idData(moveobject2)))) {
  
  indiv<-moveobject2[[i]]
  
  #trip_id<-paste(indiv$trip_id[-1], sep='_', timeLag(indiv, units='mins')<60)
  
  #indivB <- burst(x=indiv, f=trip_id)
  
  #indivB <- burst(indiv,indiv$unique_trip[-n.locs(indiv)])
  
  
  # not bursting because it is only 1 segment per individual
  dBBindiv <- brownian.bridge.dyn(indiv, raster=ras, location.error=5
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

saveRDS(p, "plotemd.RDS")

groups <- cutree(oneclust, h = 50000)



groups <- as.data.frame(group)

table(groups$group)
groups[which(groups$group == 4),]

groups$Trip_ID <- rownames(groups)
rownames(groups) <- NULL
groups <- groups[order(groups$group), ]
head(groups)

groups$Trip_ID2<-gsub("._trip_.", " _trip_ ", groups$Trip_ID)
head(groups)
groups$Trip_ID2<-gsub("\\.", "-", groups$Trip_ID2)
head(groups)


emd.gps <- gps.data %>% 
  filter(unique_trip %in% unique(groups$Trip_ID2))

emd.gps <- merge(emd.gps, groups, by.x="unique_trip", by.y="Trip_ID2", all=TRUE)

head(emd.gps)


emd.gps <- emd.gps %>% 
  filter(!is.na(lon)) 

  ggplot() +
  geom_path(data=emd.gps, aes(x = lon, y = lat, 
                               linetype=as.factor(group),
                              color = as.factor(group)
                               ), 
             cex= 0.5) +
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(data = world, aes()) + #+ #add basemap
  coord_sf(crs = 4326, xlim = range(emd.gps$lon), ylim = range(emd.gps$lat)
           )#+
  #scale_shape_manual(values=c(0, 1, 5, 6))#+xlim = range(toplot$lon), ylim = range(toplot$lat)

 #annotate(geom="text", x=-78.6, y=-8.4, label="2018", size= 13,
#        color="black") +
# guides(color = guide_legend(override.aes = list(size = 4)) ) +
#theme(
# legend.position = c(0.99, 0.99),
#legend.justification = c(1,1),
#legend.text = element_text(size = 15)

plot
unique(emd.gps$unique_trip)
