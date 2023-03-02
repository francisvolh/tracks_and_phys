#model for physiology #with Allison's input
library(lme4)
library(MuMIn)
library(tidyverse)
library(ggeffects)
library(nlme)
library(cowplot)

theme_set(theme_light())

# from SUMMARY_clean_TRIPS_FINAL
#SUMMAGPS_1trip<-readRDS(file = "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1tripFULL.RDS") #only 1 trip per bird

#glmm_tests_post<-merge(SUMMAGPS, all_deps_post, by.x="dep_id", by.y="dp_ID", all=TRUE)

#uniques_1<-unique(SUMMAGPS_1trip$unique_trip)

#glmm_Test_2spPOST1<-glmm_tests %>% 
 # filter(unique_trip %in% uniques_1)


#ONLY WITH 1 last trip (not mean trips)
#physiology and mean trip values
glmm_Test_2spPOST1<-readRDS("C:/Users/francis van Oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_post1PEBO.RDS")

#glucose
#subset only complete dataset for a particular Metabolite

# Since you only have 6 data points from 2018, I don't think you should fit 

phystestsGLUP1 <- glmm_Test_2spPOST1 %>% 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(glu)) %>% 
  filter(Spec == "PEBO")


ggplot(phystestsGLUP1, aes(y = log(glu), x =  log(sinuos), col = Year)) + geom_point()
ggplot(phystestsGLUP1, aes(y = log(glu), x =  PC1, col = Year)) + geom_point()

ggplot(phystestsGLUP1, aes(y = log(glu), x =  Year, col = Year)) + geom_violin()+ geom_point()


# Because of the small sample size in 2018, I suggest a separate Welch's t-test for a year effect
# If this is not significant, then pool years for regressions, but don't include year in the regression models
t.test(log(phystestsGLUP1$glu) ~ phystestsGLUP1$Year)

#options(na.action = "na.omit")
# Without year, you don't need to worry about unequal variance
reg0 <- lm(log(glu) ~ 1, data = phystestsGLUP1)
reg1 <- lm(log(glu) ~ log(sinuos), data = phystestsGLUP1)
reg2 <- lm(log(glu) ~ PC1, data = phystestsGLUP1) ###############DELETED method= 'ML' in this and all subsequent models
reg3 <- lm(log(glu) ~ PC1 + log(sinuos) , data = phystestsGLUP1)
reg4 <- lm(log(glu) ~ PC1 *log(sinuos),  data = phystestsGLUP1)
reg5 <- lm(log(glu) ~ PC1 *latency,  data = phystestsGLUP1)
reg6 <- lm(log(glu) ~ latency,  data = phystestsGLUP1)


options(na.action = "na.fail")
aic_lmGluP<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)

aic_lmGluP

summary(reg2)
summary(reg1)
summary(reg0)

ggplot(phystestsGLUP1, aes(y = log(glu), x =  log(sinuos))) + geom_point()
ggplot(phystestsGLUP1, aes(y = log(glu), x =  PC1)) + geom_point()


#do I need to check for model fit and assumptions if the next best is the NULL with less than 2 dAIC??
# It's probably a good idea to check that your other models aren't being really thrown off by something,
# but you don't need to worry about it too much

#write.csv(aic_lmGluP, "AIC.mean.gluPEBO.csv")
#best model is .39 delta AIC from the null model


# ------
#chol

phystestsCHOLP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(chol)) %>% 
  filter(Spec == "PEBO")

ggplot(phystestsCHOLP1, aes(y = log(chol), x =  log(sinuos), col = Year)) + geom_point()
ggplot(phystestsCHOLP1, aes(y = log(chol), x =  PC1, col = Year)) + geom_point()
ggplot(phystestsCHOLP1, aes(y = log(chol), x =  Year, col = Year)) + geom_violin()+ geom_point()

# Same here - check for year effect before fitting models without year
t.test(log(phystestsCHOLP1$chol) ~ phystestsCHOLP1$Year)

phystestsCHOLP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(chol)) %>% 
  filter(Spec == "PEBO") %>% 
  filter(Year == "2019")

options(na.action = "na.omit")

reg0 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~  1)
reg1 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ log(sinuos))
reg2 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ PC1)
reg3 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ log(sinuos) + PC1)
reg4 <- lm(data = phystestsCHOLP1, formula =  log(chol) ~ log(sinuos) * PC1)
reg5 <- lm(data = phystestsCHOLP1, log(chol) ~ PC1 *log (latency))
reg6 <- lm(data = phystestsCHOLP1, log(chol) ~ log (latency))

#options(na.action = "na.fail")
aic_lmCholP<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmCholP
#write.csv(aic_lmCholP, "AIC.mean.cholPEBO.csv")

ggplot(phystestsCHOLP1, aes(y = log(chol), x =  log(sinuos))) + geom_point()
ggplot(phystestsCHOLP1, aes(y = log(chol), x =  PC1)) + geom_point()

summary(reg4)
summary(reg2)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(reg1)
par(opar)
hist(resid(reg1))


meanModPEBOchol1trip<-ggpredict(
  reg4,
  terms = c("sinuos"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)

PEBOchol1trip <- ggplot()+
  geom_smooth(data=ggpredict(
    reg4,
    terms = c("sinuos"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ),
  aes(x = (x), y = (predicted), color = group),
  method = "lm"
  )+
  xlab("Sinuosity (log)")+
  ylab("Cholesterol (log)")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  theme_bw()

meanModPEBOchol1tripPC1<-ggpredict(
  reg4,
  terms = c("PC1"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)

PEBOchol1tripPC1 <- ggplot()+
  geom_smooth(data=ggpredict(
    reg4,
    terms = c("PC1"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ),
  aes(x = (x), y = (predicted), color = group),
  method = "lm"
  )+
  xlab("PC1")+
  ylab("Cholesterol (log)")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  theme_bw()

#trig
phystestsTRIP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(tri)) %>% 
  filter(Spec == "PEBO")
ggplot(phystestsTRIP1, aes(y = log(tri), x =  log(sinuos), col = Year)) + geom_point()
ggplot(phystestsTRIP1, aes(y = log(tri), x =  PC1, col = Year)) + geom_point()
ggplot(phystestsTRIP1, aes(y = log(tri), x =  Year, col = Year)) + geom_violin()+ geom_point()


t.test(log(phystestsTRIP1$tri) ~ phystestsTRIP1$Year)

phystestsTRIP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(tri)) %>% 
  filter(Spec == "PEBO") %>% 
  filter(latency < 20) # done with removing and not removing outlier in latency, basically same results


options(na.action = "na.omit")
reg0 <- lm(data = phystestsTRIP1, formula =  log(tri) ~  1)
reg1 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ log(sinuos))
reg2 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ PC1)
reg3 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ log(sinuos) + PC1)
reg4 <- lm(data = phystestsTRIP1, formula =  log(tri) ~ log(sinuos) * PC1)
reg5 <- lm(data = phystestsTRIP1, log(tri) ~ PC1 * log(latency))
reg6 <- lm(data = phystestsTRIP1, log(tri) ~ log(latency))

aic_lmTriP1<-MuMIn::model.sel(reg0,reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmTriP1

ggplot(phystestsTRIP1, aes(y = log(tri), x = latency)) + geom_point()


meanModPEBOtri1trip<-ggpredict(
  reg6,
  terms = c("latency"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)


PEBOtri1trip <- ggplot()+
  geom_smooth(data=ggpredict(
    reg6,
    terms = c("latency"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ),
  aes(x = (x), y = (predicted)),
  method = "lm"
  )+
  xlab("Latency (log)")+
  ylab("Triglycerides (log)")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  theme_bw()

###KETONES
phystestsKETP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(ket)) %>% 
  filter(Spec == "PEBO")

t.test(log(phystestsKETP1$ket) ~ phystestsKETP1$Year)

phystestsKETP1<-glmm_Test_2spPOST1 %>% 
  select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(ket)) %>% 
  filter(Spec == "PEBO") %>% 
  filter(Year =="2019")

options(na.action = "na.omit")
reg0 <- lm(data = phystestsKETP1, formula =  log(ket) ~  1)
reg1 <- lm(data = phystestsKETP1, formula =  log(ket) ~ log(sinuos))
reg2 <- lm(data = phystestsKETP1, formula =  log(ket) ~ PC1)
reg3 <- lm(data = phystestsKETP1, formula =  log(ket) ~ log(sinuos) + PC1)
reg4 <- lm(data = phystestsKETP1, formula =  log(ket) ~ log(sinuos) * PC1)
reg5 <- lm(data = phystestsKETP1, log(ket) ~ PC1 * log(latency))
reg6 <- lm(data = phystestsKETP1, log(ket) ~ log(latency))

#options(na.action = "na.fail")
aic_lmKetP1<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)
aic_lmKetP1

