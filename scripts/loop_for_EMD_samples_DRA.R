library(move)
library(foreach)
library(parallel)
library(doParallel)


ncores <- Sys.getenv("SLURM_CPUS_PER_TASK")

registerDoParallel(cores=ncores)# Shows the number of Parallel Workers to be used

print(ncores) # this how many cores are available, and how many you have requested.

getDoParWorkers()# you can compare with the number of actual workers


#clean run for CC with all birds, 10 steps per trips (previous run had 30 and lost 4 birds/trips)
moveobjectall<-readRDS("moveobjectALL.RDS")

#list of emd matrices
emdlist <- list()

#how many loops of analysis I want

sample_runs <- 10

all_ids<-unique(moveobjectall@trackId)

#loop to select individuals

foreach (
  i=1:sample_runs,
  #.combine = 'rbind',
  .packages = c("move")) %dopar% {
    
    ten_ids <- sample(all_ids, 10)
    
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
    
    emdlist[[i]] <-emdDists
  }
  
saveRDS(emdlist, "emdlist_trial1.RDS")  