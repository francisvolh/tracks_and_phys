#model for physiology #with Allison's input

#install.packages("MuMIn", repos="https://r-forge.r-project.org/", type = "source")
#library(MuMIn)
#library(tidyverse)
#library(ggeffects)
#library(nlme)
#library(cowplot)

ggplot2::theme_set(ggplot2::theme_light())

{
### Loading glmm_test for metrics and merge with deployment/nutrient data

#original deployment csvs fused into 1 (2018, and 2019 gps, cams, and axxy)
all_deps<-readRDS("data/all_deployments.phys.RDS")


#glmm with clean trips, Sinuosity, and  PCA calculations already both species
glmm_tests<-readRDS("data/glmm_testsPCAv2.RDS")



#LMM model
# Full model: log10_TotalTime ~ lo10_MaxDistColony-SCALED + logTOTAL PATH_SCALED + TORT + Spec + Year + Year*Spec + (individual)
# maybe to incorporate sex into SUMMAGPS

#merge trip summaries with Phys Sheet including sinuosity info a V1
glmm_tests<-merge(glmm_tests, all_deps, by.x="ID", by.y="dp_ID", all=TRUE)

### get julian date
glmm_tests$julian <- format(glmm_tests$startt, "%j") 

###
glmm_tests <- glmm_tests |>
  dplyr::filter(!is.na(Year)) |>
  #dplyr::filter(!tottime >20)|>#excluding trip of one bird that was tracked for 9 days
  dplyr::rename(dep_id =ID)|>
  dplyr::mutate(
    julian = as.numeric(julian),
    Year=as.factor(Year),
    Spec =as.factor(Spec),
    TimeTrip =as.numeric(TimeTrip),
    ID=as.factor(dep_id),
    sex=as.factor(sex),
    indx_TripD_MaxD =as.numeric(TimeTrip)/ maxdist,
    log_ind = log(indx_TripD_MaxD),
    log_Time = log(as.numeric(TimeTrip)),
    log_MaxDist = log(maxdist),
    log_TotDist = log(totdist),
    latency = as.numeric(difftime(timeR, endt, units = "hours"))
  ) 



#glmm_tests|>
 # dplyr::select(ID, latency)|>
  #View()

#glmm_tests|>
 # View()
a<-glmm_tests |>
  dplyr::filter(Spec == "GUCO") |>
  dplyr::group_by(dep_id) |> 
  dplyr::filter(trip_id == min(trip_id)) #closest to sampling for GUCO

b<-glmm_tests |>
  dplyr::filter(Spec == "PEBO") |>
  dplyr::group_by(dep_id) |> 
  dplyr::filter(trip_id == max(trip_id)) #closest to sampling for PEBO

SUMMAGPS_1trip<-rbind(a,b)
}

glmm_Test_2spPOST1<-SUMMAGPS_1trip
#glucose
#subset only complete dataset for a particular Metabolite

# Since you only have 6 data points from 2018, I don't think you should fit 
phystestsGLUP1 <- glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(glu)) |> 
  dplyr::filter(Spec == "PEBO")


ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  log(sinuos), col = Year)) + 
  ggplot2::geom_point()

ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  PC1, col = Year)) + 
  ggplot2::geom_point()

ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  Year, col = Year)) + 
  ggplot2::geom_violin()+ 
  ggplot2::geom_point()


# Because of the small sample size in 2018, I suggest a separate Welch's t-test for a year effect
# If this is not significant, then pool years for regressions, but don't include year in the regression models
t.test(log(phystestsGLUP1$glu) ~ phystestsGLUP1$Year)

options(na.action = "na.omit")
# Without year, you don't need to worry about unequal variance ###############DELETED method= 'ML' in this and all subsequent models
reg0 <- lm(data = phystestsGLUP1, formula =log(glu) ~ 1)
reg1 <- lm(data = phystestsGLUP1, formula =log(glu) ~ sinuos)
reg2 <- lm(data = phystestsGLUP1, formula =log(glu) ~ PC1) 
reg3 <- lm(data = phystestsGLUP1, formula =log(glu) ~ PC1+(sinuos) )
reg4 <- lm(data = phystestsGLUP1, formula =log(glu) ~ PC1*(sinuos))
reg5 <- lm(data = phystestsGLUP1, formula =log(glu) ~ PC1*log(latency))
reg6 <- lm(data = phystestsGLUP1, formula =log(glu) ~ log(latency))

options(na.action = "na.fail")
aic_lmGluP<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)

aic_lmGluP

summary(reg2)
summary(reg1)
summary(reg0)

ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  log(sinuos))) +
  ggplot2::geom_point()
ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  PC1)) + 
  ggplot2::geom_point()

ggplot2::ggplot(phystestsGLUP1, ggplot2::aes(y = log(glu), x =  Year, col = Year)) + ggplot2::geom_violin()+
  ggplot2::geom_point()


#write.csv(aic_lmGluP, "AIC.mean.gluPEBO.csv")
#best model is 1.58 delta AIC from the null model

#no save plot for GLU

# ------
#chol

phystestsCHOLP1<-glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(chol)) |> 
  dplyr::filter(Spec == "PEBO")

ggplot2::ggplot(phystestsCHOLP1, ggplot2::aes(y = log(chol), x =  log(sinuos), col = Year)) + ggplot2::geom_point()
ggplot2::ggplot(phystestsCHOLP1, ggplot2::aes(y = log(chol), x =  PC1, col = Year)) + ggplot2::geom_point()
ggplot2::ggplot(phystestsCHOLP1, ggplot2::aes(y = log(chol), x =  Year, col = Year)) + ggplot2::geom_violin()+ ggplot2::geom_point()

# Same here - check for year effect before fitting models without year
t.test(log(phystestsCHOLP1$chol) ~ phystestsCHOLP1$Year)

phystestsCHOLP1<-glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(chol)) |> 
  dplyr::filter(Spec == "PEBO") #|> 
  #filter(Year == "2019")

options(na.action = "na.omit")

reg0 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~  1)
reg1 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ (sinuos))
reg2 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ PC1)
reg3 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ PC1+(sinuos) )
reg4 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ PC1*(sinuos))
reg5 <- lm(data = phystestsCHOLP1, formula = log(chol) ~ PC1*log(latency))
reg6 <- lm(data = phystestsCHOLP1, formula = log(chol) ~ log(latency))

#options(na.action = "na.fail")
aic_lmCholP<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmCholP
aic_lmCholP_df<- as.data.frame(aic_lmCholP)
aic_lmCholP_df$model <- "aic_lmCholP" 
#write.csv(aic_lmCholP_df, "data/AIC.1trip.cholPEBOv2.csv")

ggplot2::ggplot(phystestsCHOLP1, ggplot2::aes(y = log(chol), x =  log(sinuos))) + ggplot2::geom_point()
ggplot2::ggplot(phystestsCHOLP1, ggplot2::aes(y = log(chol), x =  PC1)) + ggplot2::geom_point()

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(reg1)
par(opar)
hist(resid(reg1))
par(opar)

meanModPEBOchol1trip<-ggeffects::ggpredict(
  reg1,
  terms = c("sinuos"),
  ci_lvl = 0.95,
  type = "fe",
  back_transform= FALSE, 
  typical = "mean"
)

PEBOchol1trip<-plot(meanModPEBOchol1trip, show_data = TRUE)+
  ggplot2::xlab("Sinuosity (log)")+
  ggplot2::ylab("Log-Cholesterol (mg/dL)")+
  ggplot2::ggtitle(NULL)






#trig
phystestsTRIP1<-glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(tri)) |> 
  dplyr::filter(Spec == "PEBO")
ggplot2::ggplot(phystestsTRIP1,  ggplot2::aes(y = log(tri), x =  (sinuos), col = Year)) +  ggplot2::geom_point()
ggplot2::ggplot(phystestsTRIP1,  ggplot2::aes(y = log(tri), x =  PC1, col = Year)) +  ggplot2::geom_point()
ggplot2::ggplot(phystestsTRIP1,  ggplot2::aes(y = log(tri), x =  Year, col = Year)) +  ggplot2::geom_violin()+  ggplot2::geom_point()


t.test(log(phystestsTRIP1$tri) ~ phystestsTRIP1$Year)

phystestsTRIP1<-glmm_Test_2spPOST1 |> 
  dplyr:: select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(tri)) |> 
  dplyr::filter(Spec == "PEBO") |> 
  dplyr:: filter(latency < 20) # done removing and not removing outlier in latency, basically same results


options(na.action = "na.omit")
reg0 <- lm(data = phystestsTRIP1, formula =  log(tri) ~  1)
reg1 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ (sinuos))
reg2 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ PC1)
reg3 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ PC1 + (sinuos) )
reg4 <- lm(data = phystestsTRIP1, formula =  log(tri) ~  PC1 * (sinuos) )
reg5 <- lm(data = phystestsTRIP1, log(tri) ~ PC1 * log(latency))
reg6 <- lm(data = phystestsTRIP1, log(tri) ~ log(latency))

options(na.action = "na.fail")

aic_lmTriP1<-MuMIn::model.sel(reg0,reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmTriP1


aic_lmTriP1_df<- as.data.frame(aic_lmTriP1)
aic_lmTriP1_df$model<-"aic_lmTriP1"
#write.csv(aic_lmTriP1_df, "data/AIC.1trip.triPEBOv2.csv")
summary(reg6)

meanModPEBOtri1trip<-ggeffects::ggpredict(
  reg6,
  terms = c("latency"),
  ci_lvl = 0.95,
  type = "fe",
  back_transform= FALSE, 
  typical = "mean"
)

PEBOtri1tripA <-plot(meanModPEBOtri1trip, show_data = TRUE)+
  ggplot2::xlab("Latency (log)")+
  ggplot2::ylab("Log-Triglycerides (mg/dL)")+
  ggplot2::ggtitle(NULL)+
  ggplot2::scale_x_continuous(#breaks = 3:5, 
    limits = c(3, 20))
  


###KETONES
phystestsKETP1<-glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(ket)) |> 
  dplyr::filter(Spec == "PEBO")

t.test(log(phystestsKETP1$ket) ~ phystestsKETP1$Year)

phystestsKETP1<-glmm_Test_2spPOST1 |> 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) |> 
  dplyr::filter(!is.na(ket)) |> 
  dplyr::filter(Spec == "PEBO") |> 
  dplyr::filter(Year =="2019")

options(na.action = "na.omit")
reg0 <- lm(data = phystestsKETP1, formula =  log(ket) ~  1)
reg1 <- lm(data = phystestsKETP1, formula =  log(ket) ~ (sinuos))
reg2 <- lm(data = phystestsKETP1, formula =  log(ket) ~ PC1)
reg3 <- lm(data = phystestsKETP1, formula =  log(ket) ~ (sinuos) + PC1)
reg4 <- lm(data = phystestsKETP1, formula =  log(ket) ~ (sinuos) * PC1)
reg5 <- lm(data = phystestsKETP1, log(ket) ~ PC1 * log(latency))
reg6 <- lm(data = phystestsKETP1, log(ket) ~ log(latency))

#options(na.action = "na.fail")
aic_lmKetP1<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmKetP1
#null within 2 delta aic

# no save model

#save two nutrient with rels
phys_effotr_plots <- cowplot::plot_grid(PEBOchol1trip, PEBOtri1tripA, nrow = 1, labels = c("A", "B"))
ggplot2::ggsave(phys_effotr_plots, filename = "plots/phys_effort_plotsv2.png", width = 10, height = 5, units = "in", dpi = 300, bg ="white")

## save predictions for paper????
