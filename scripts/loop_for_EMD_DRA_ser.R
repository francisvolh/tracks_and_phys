library(move)

#clean run for CC with all birds, 10 steps per trips (previous run had 30 and lost 4 birds/trips)
moveobjectall<-readRDS("moveobjectALL.RDS")
#moveobjectall<-readRDS(file.choose())


#list of emd matrices
emdlist <- list()

#how many loops of analysis I want

sample_runs <- 3  ### CYCLES MODIFY ACCORDINGLY

all_ids<-unique(moveobjectall@trackId)

#loop to select individuals

for(k in 1:sample_runs){
  
  print(paste('Start run/cycle',k,'at', format(Sys.time(), "%T")))
  
  ten_ids <- sample(all_ids, 3) ### MODIFY ACCORDINGLY
  
  moveobject <- moveobjectall[[which(namesIndiv(moveobjectall)%in% ten_ids)]]
  
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
  
  print(paste('Finished run/cycle',k,'at', format(Sys.time(), "%T")))
  
  emdlist[[k]] <-emdDists
}

saveRDS(emdlist, "emdlist_trial1.RDS")



