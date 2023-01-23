#cowplot of all birds highlighting physiology selected trip

gps.data$colorTrip <- "black"

gps.data <- gps.data %>% 
  filter (dep_id %in% unique(glmm_Test_2spPOST1$dep_id)) %>% 
  mutate(
    colorTrip = case_when(
      unique_trip %in% unique(glmm_Test_2spPOST1$unique_trip)~ "red"
    )
  )

plotsList <- list()
for (i in unique(gps.data$dep_id)) {
  
  one_dep <- gps.data %>% 
    filter(dep_id == i)
  
  plotsList[[i]] <- ggplot(one_dep)+
    geom_point(aes(x = time, y = coldist, color=colorTrip), cex= 0.5)+
    theme(legend.position = "none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    xlab(NULL)+
    ggtitle(i)
  
  #print(p)
  #readline('next')


  }

n <- length(plotsList)
nCol <- floor(sqrt(n))
library(cowplot)
do.call("plot_grid", c(plotsList, ncol=nCol))
