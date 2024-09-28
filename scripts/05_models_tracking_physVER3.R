#models for tracking and phys

###
#Models for hypothesis 1: foraging effort and years, sp, and sex
library(lme4)
library(nlme)
library(MuMIn)
library(ggplot2)
library(dplyr)
library(GGally)
library(ggeffects)
library(cowplot)
library(lattice)
library(factoextra)
library(ggpubr)

#setwd("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/")

#original deployment csvs fused into 1 (2018, and 2019 gps, cams, and axxy)
all_deps<-readRDS("data/all_deployments.phys.RDS")
#"E:/05BACKUP July 10 2024/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/tracks_and_phys/tracks_and_phys/data"

#full trips
#SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")

#without overnight trips
#SUMMAGPSshort<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPSshort.csv") #file without 1-5 pointer trips, no outliers in duration, NO trips less than 5km from Col


#SUMMAGPS_1trip<-read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS_1trip.csv")


#glmm with clean trips, Sinuosity, and  PCA calculations already both species
glmm_tests<-readRDS("E:/05BACKUP July 10 2024/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_testsPCA.RDS")


#LMM model
# Full model: log10_TotalTime ~ lo10_MaxDistColony-SCALED + logTOTAL PATH_SCALED + TORT + Spec + Year + Year*Spec + (individual)
# maybe to incorporate sex into SUMMAGPS

#merge trip summaries with Phys Sheet including sinuosity info a V1
glmm_tests<-merge(glmm_tests, all_deps, by.x="dep_id", by.y="dp_ID", all=TRUE)

### get julian date
glmm_tests$julian <- format(glmm_tests$startt, "%j") 

###
glmm_tests <- glmm_tests |>
  dplyr::filter(!is.na(Year)) |>
  #dplyr::filter(!tottime >20)|>#excluding trip of one bird that was tracked for 9 days
  dplyr::mutate(
    julian = as.numeric(julian),
    Year=as.factor(Year),
    TimeTrip =as.numeric(TimeTrip),
    dep_id=as.factor(dep_id),
    sex=as.factor(sex),
    indx_TripD_MaxD =as.numeric(TimeTrip)/ maxdist,
    log_ind = log(indx_TripD_MaxD),
    log_Time = log(as.numeric(TimeTrip)),
    log_MaxDist = log(maxdist),
    log_TotDist = log(totdist)
  ) 

#saveRDS(glmm_tests, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_testsMods.RDS")

#glmm_tests<-readRDS("C:/Users/francis van Oordt/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_tests.RDS")


#ggpairs(glmm_tests[,c("TimeTrip", "maxdist", "totdist","sinuos", "SP")], aes(colour = SP), progress = FALSE) +
# theme_bw()

#obtain mean duration of trip value per year (median to avoid a couple of extremes)
#for Axxy paper to estimate energy away with time budgets

glmm_tests|>
  dplyr::group_by(dep_id, Day, Year)|>
  dplyr::summarise(
    total_Trip = sum(tottime)
  )|>
  dplyr::ungroup()|>
  dplyr::group_by(Year)|>
  dplyr::summarise(
    meanAway = median(total_Trip)
  )

#check relationships of increased parameters in time pass by (days) totdist, TimeTrip, maxdist, sinous
plot<- ggpubr::ggscatter(glmm_tests, y = "TimeTrip", x = "julian",
                         color = "black", shape = 21, #size = 3, # Points color, shape and size
                         add = "reg.line",  # Add regressin line
                         add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                         conf.int = TRUE, # Add confidence interval
                         cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                         cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                               label.sep = "\n")
)
ggpubr::facet(plot, facet.by = "Year", scales = "free")

###################################################################################
#models with covariate and fixed effects, fixed slopes
# latest version, all models with nlme::lme

#TOTAL TRIP DURATION time trip

#options(na.action = "na.omit")
#glmm_TripDur <- lme4::lmer(log(TimeTrip) ~ #TimeTrip log_Time
#                    Year*Spec +
#                   (1 | dep_id),
#                REML = FALSE,  
#               data = glmm_tests)

#summary(glmm_TripDur)
#plot(glmm_TripDur, type = c("p", "smooth"))
#plot(glmm_TripDur, sqrt(abs(resid(.))) ~ fitted(.), type =c("p", "smooth"))

#plot(resid(glmm_TripDur),log(glmm_tests$TimeTrip))
#lattice::qqmath(glmm_TripDur, id=0.05)
#boxplot(resid(glmm_TripDur)~ interaction(glmm_tests$Year, glmm_tests$Spec))

#car::leveneTest(resid(glmm_TripDur), interaction(glmm_tests$Year, glmm_tests$Spec))
#car::influencePlot(glmm_TripDur)



options(na.action = "na.omit")
glmm_TripDur <- nlme::lme( log(TimeTrip) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|dep_id, 
                           method = "ML",
                           weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                           data = glmm_tests)

summary(glmm_TripDur)
plot(glmm_TripDur)
plot(resid(glmm_TripDur),log(glmm_tests$TimeTrip))
qqnorm(glmm_TripDur, ~ residuals(., type = "p"), abline = c(0, 1))

car::leveneTest(residuals(glmm_TripDur, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))

boxplot(residuals(glmm_TripDur, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))


options(na.action = "na.fail")

lme.formula <- nlme::lme.formula # only need to call the function once

aic_TripDur<-MuMIn::dredge(global.model = glmm_TripDur )
aic_TripDur

#best model of Trip Duration (time): Year only
options(na.action = "na.omit")
glmm_TripDurB <- nlme::lme( log(TimeTrip) ~ #log_TotDist totdist
                              Year, 
                            random = ~1|dep_id, 
                            method = "REML",
                            weights = nlme::varIdent(form = ~ 1 | Year),
                            data = glmm_tests[order(glmm_tests$Spec),])


summary(glmm_TripDurB)

plot(glmm_TripDurB)
plot(residuals(glmm_TripDurB, type = "pearson"), log(glmm_tests$TimeTrip))
qqnorm(glmm_TripDurB, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(glmm_TripDurB, type = "pearson")~ interaction(glmm_tests$Year))


TripDurBmeans<-ggeffects::ggemmeans(model = glmm_TripDurB, 
                                    terms = c('Year'),
                                    ci.lvl = 0.95,
                                    type = "fe",
                                    typical = "mean",
                                    condition = NULL,
                                    back.transform = FALSE,
                                    interval = "confidence")

#plots 
#TIMETRIP YEAR AND SPECIES
A2<-ggplot2::ggplot(data= glmm_tests, ggplot2::aes(x=Year, y=log(TimeTrip), color = Year))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
                      alpha = 0.3)+
  ggplot2::scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = glmm_TripDurB, 
                                                     terms = c('Year'),
                                                     ci_lvl = 0.95,
                                                     type = "fe",
                                                     typical = "mean",
                                                     back_transform = FALSE,
                                                     condition = NULL) ,
                           ggplot2::aes(x = x, y = predicted, 
                                        ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Total trip duration (log)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()



#TOTAL DISTANCE trip path
options(na.action = "na.omit")
glmm_TripPath <- lme4::lmer( log(totdist) ~ #log_TotDist totdist
                               Year*Spec +
                               (1 | dep_id),
                             REML = FALSE,  
                             data = glmm_tests)

options(na.action = "na.fail")
aic_TripPath<-MuMIn::dredge(glmm_TripPath)
aic_TripPath

plot(glmm_TripPath)
plot(resid(glmm_TripPath),log(glmm_tests$totdist))
lattice::qqmath(glmm_TripPath, id=0.05)
boxplot(resid(glmm_TripPath)~ interaction(glmm_tests$Year, glmm_tests$Spec))
car::leveneTest(resid(glmm_TripPath), interaction(glmm_tests$Year, glmm_tests$Spec))



glmm_TripPath <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                              Year*Spec, 
                            random = ~1|dep_id, 
                            method = "ML",
                            weights =  nlme::varIdent(form = ~ 1 | Year*Spec),
                            data = glmm_tests)

summary(glmm_TripPath)
car::leveneTest(residuals(glmm_TripPath, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
plot(glmm_TripPath)
plot(resid(glmm_TripPath),log(glmm_tests$totdist))

boxplot(resid(glmm_TripPath)~ interaction(glmm_tests$Year, glmm_tests$Spec))
qqnorm(glmm_TripPath, ~ residuals(., type = "p"), abline = c(0, 1))
options(na.action = "na.fail")
aic_TripPath<-MuMIn::dredge(glmm_TripPath)
aic_TripPath

glmm_TripPathB <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                               Spec, 
                             random = ~1|dep_id, 
                             method = "REML",
                             weights = nlme::varIdent(form = ~ 1 | Spec),
                             data = glmm_tests[order(glmm_tests$Spec),])

summary(glmm_TripPathB)
car::leveneTest(residuals(glmm_TripPathB, type = "pearson")~interaction(glmm_tests$Spec))
plot(glmm_TripPathB)

plot(residuals(glmm_TripPathB),log(glmm_tests$totdist))

boxplot(residuals(glmm_TripPathB, type = "pearson")~ interaction(glmm_tests$Spec))

qqnorm(glmm_TripPathB, ~ residuals(., type = "p"), abline = c(0, 1))


TripPathBmeans<-ggeffects::ggemmeans(model = glmm_TripPathB, 
                                     terms = c('Spec'),
                                     ci.lvl = 0.95,
                                     type = "fe",
                                     typical = "mean",
                                     condition = NULL,
                                     back.transform = FALSE,
                                     interval = "confidence")




B2<-ggplot(data= glmm_tests, aes(x=Spec, y=log(totdist), color = Spec ))+ 
  geom_point(position  = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  geom_pointrange(data=ggeffects::ggemmeans(model = glmm_TripPathB, 
                                            terms = c('Spec'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back.transform = FALSE) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, 
                      ymax = predicted+std.error, 
                      color = x),
                  position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1)
  )+
  xlab(NULL)+
  ylab("Total trip path")+
  guides(color = "none")+
  theme_bw()

#to include Year and Species as second best model

glmm_TripPathA <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                               Spec+Year, 
                             random = ~1|dep_id, 
                             method = "REML",
                             weights =  nlme::varIdent(form = ~ 1 | Spec),
                             data = glmm_tests[order(glmm_tests$Spec),])

summary(glmm_TripPathA)
car::leveneTest(residuals(glmm_TripPathA, type = "pearson")~interaction(glmm_tests$Spec))
plot(glmm_TripPathB)

plot(residuals(glmm_TripPathA),log(glmm_tests$totdist))

boxplot(residuals(glmm_TripPathA, type = "pearson")~ interaction(glmm_tests$Year,glmm_tests$Spec))

qqnorm(glmm_TripPathA, ~ residuals(., type = "p"), abline = c(0, 1))


TripPathAmeans<-ggeffects::ggemmeans(model = glmm_TripPathA, 
                                     terms = c('Year','Spec'),
                                     ci.lvl = 0.95,
                                     type = "fe",
                                     typical = "mean",
                                     condition = NULL,
                                     back.transform = FALSE,
                                     interval = "confidence")




B2A<-ggplot(data= glmm_tests, aes(x=Year, y=log(totdist), color = Spec ))+ 
  geom_point(position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1),
             alpha = 0.3)+
  geom_pointrange(data=ggeffects::ggemmeans(model = glmm_TripPathA, 
                                            terms = c('Year', 'Spec'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back.transform = FALSE,
                                            interval = "confidence"),
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, 
                      ymax = predicted+std.error, 
                      color = group),
                  position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1) )+#,, linetype = facet
  #method = "lm"
  xlab(NULL)+
  ylab("Total trip path (log)")+
  guides(color = "none")+
  theme_bw()
B2A


#MAX DISTANCE
options(na.action = "na.omit")
glmm_MaxDist <- lme4::lmer( log(maxdist) ~  #log_MaxDist maxdist
                              Year*Spec +
                              (1 | dep_id),
                            REML = FALSE,  
                            data = glmm_tests)

options(na.action = "na.fail")
aic_MaxDist<-MuMIn::dredge(glmm_MaxDist)
aic_MaxDist
plot(glmm_MaxDist)
plot(resid(glmm_MaxDist), log(glmm_tests$maxdist))
lattice::qqmath(glmm_MaxDist, id=0.05)
boxplot(resid(glmm_MaxDist)~ interaction(glmm_tests$Year, glmm_tests$Spec))
car::leveneTest(resid(glmm_MaxDist), interaction(glmm_tests$Year, glmm_tests$Spec))



glmm_MaxDist <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|dep_id, 
                           method = "ML",
                           weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                           data = glmm_tests)

summary(glmm_MaxDist)
car::leveneTest(residuals(glmm_MaxDist, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
plot(glmm_MaxDist)
plot(resid(glmm_MaxDist),log(glmm_tests$maxdist))

boxplot(residuals(glmm_MaxDist, type= "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

qqnorm(glmm_MaxDist, ~ residuals(., type = "p"), abline = c(0, 1))
options(na.action = "na.fail")
aic_MaxDist<-MuMIn::dredge(glmm_MaxDist)
aic_MaxDist

#best parsim model for max dist
glmm_MaxDistB <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                              Spec, 
                            random = ~1|dep_id, 
                            method = "REML",
                            weights = nlme::varIdent(form = ~ 1 | Spec),
                            data = glmm_tests[order(glmm_tests$Spec),])

car::leveneTest(residuals(glmm_MaxDistB, type = "pearson"), interaction(glmm_tests$Spec))
plot(glmm_MaxDistB)
plot(resid(glmm_MaxDistB),log(glmm_tests$maxdist))

boxplot(residuals(glmm_MaxDistB, type= "pearson")~ interaction( glmm_tests$Spec))

MaxDistB<-ggeffects::ggemmeans(model = glmm_MaxDistB, 
                               terms = c('Spec'),
                               ci.lvl = 0.95,
                               type = "fe",
                               typical = "mean",
                               condition = NULL,
                               back.transform = FALSE,
                               interval = "confidence")

C2<-ggplot(data= glmm_tests, aes(x=Spec, y=log(maxdist), color = Spec ))+ 
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  geom_pointrange(data=ggeffects::ggemmeans(model = glmm_MaxDistB, 
                                            terms = c('Spec'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back.transform = FALSE) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Max. distance from the colony")+
  guides(color = "none")+
  theme_bw()

C2


#to plot model with year and spec for max dist

glmm_MaxDistA <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                              Spec + Year, 
                            random = ~1|dep_id, 
                            method = "REML",
                            weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                            data = glmm_tests[order(glmm_tests$Spec),])

car::leveneTest(residuals(glmm_MaxDistB, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
plot(glmm_MaxDistB)
plot(resid(glmm_MaxDistB),log(glmm_tests$maxdist))

boxplot(residuals(glmm_MaxDistB, type= "pearson")~ interaction( glmm_tests$Spec, glmm_tests$Year))

MaxDistA<-ggeffects::ggemmeans(model = glmm_MaxDistA, 
                               terms = c('Spec', 'Year'),
                               ci.lvl = 0.95,
                               type = "fe",
                               typical = "mean",
                               condition = NULL,
                               back.transform = FALSE,
                               interval = "confidence")

C2A<-ggplot(data= glmm_tests, aes(x=Year, y=log(maxdist), color = Spec ))+ 
  geom_point(position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1),
             alpha = 0.3)+
  geom_pointrange(data=ggeffects::ggemmeans(model = glmm_MaxDistA, 
                                            terms = c('Year', 'Spec'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back.transform = FALSE,
                                            interval = "confidence") ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, 
                      color = group), 
                  position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1) )+#,, linetype = facet
  #method = "lm")+
  xlab(NULL)+
  ylab("Max. distance from the colony (log)")+
  guides(color = "none")+
  theme_bw()

C2A


#SINUOSITY
options(na.action = "na.omit")
glmm_sin <- lme4::lmer( log(sinuos) ~ #sinuos log10(sinuos)
                          Year*Spec +
                          (1 | dep_id),
                        REML = FALSE,  
                        data = glmm_tests)

options(na.action = "na.fail")
aic_sin<-MuMIn::dredge(glmm_sin)
aic_sin

summary(glmm_sin)
plot(glmm_sin)
plot(resid(glmm_sin), log(glmm_tests$sinuos))
lattice::qqmath(glmm_sin, id=0.05)


boxplot(resid(glmm_sin)~ interaction(glmm_tests$Year, glmm_tests$Spec))
car::leveneTest(resid(glmm_sin), interaction(glmm_tests$Year, glmm_tests$Spec))


#glmm_sin <- lme( log(sinuos) ~ #log_TotDist totdist
#                    Spec*Year, 
#                  random = ~1|dep_id, 
#                  method = "ML",
#                 weights = nlme::varIdent(form = ~ 1 | Spec*Year),
#                data = glmm_tests)

#summary(glmm_sin)
#car::leveneTest(residuals(glmm_sin, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
#plot(glmm_sin)

#plot(resid(glmm_sin),log(glmm_tests$sinuos))
#boxplot(residuals(glmm_sin, type= "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

#qqnorm(glmm_sin, ~ residuals(., type = "p"), abline = c(0, 1))

#options(na.action = "na.fail")

#aic_sin<-MuMIn::dredge(glmm_sin)
#aic_sin

glmm_sin <- nlme::lme( log(sinuos) ~ #log_TotDist totdist
                         Spec*Year, 
                       random = ~1|dep_id, 
                       method = "REML",
                       weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                       data = glmm_tests[order(glmm_tests$Spec),])

sinmeans<-ggeffects::ggemmeans(model = glmm_sin, 
                               terms = c('Spec', 'Year'),
                               ci.lvl = 0.95,
                               type = "fe",
                               typical = "mean",
                               condition = NULL,
                               back.transform = FALSE,
                               interval = "confidence")


##
D2<-ggplot(data= glmm_tests, aes(x=Year, y=log(sinuos), color = Spec))+ #, linetype = Year
  geom_point(position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1),
             alpha = 0.3)+
  geom_pointrange(data=ggeffects::ggemmeans(model = glmm_sin, 
                                            terms = c('Year', 'Spec'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back.transform = FALSE,
                                            interval = "confidence") ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error,
                      color = group), 
                  position = position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1) )+#,, linetype = facet
  #method = "lm"
  
  xlab(NULL)+
  ylab("Sinuosity")+
  guides(color = "none")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  theme_bw()
D2

pcol2<-cowplot::plot_grid(A2, B2A, C2A, D2, labels = c('A', 'B', 'C', 'D'), ncol = 2)
ggplot2::ggsave("plots/for_effort_metrics1.png", pcol2, dpi = 300, bg = "white", units = 'in', width = 10, height = 6.5)


mod1<- as.data.frame(aic_TripDur)
mod2<-as.data.frame(aic_TripPath)
mod3<-as.data.frame(aic_MaxDist)
mod4<-as.data.frame(aic_sin)

mod1$model<-"aic_TripDur"
mod2$model<-"aic_TripPath"
mod3$model<-"aic_MaxDist"
mod4$model<-"aic_sin"

all_mods<-rbind(mod1, mod2, mod3, mod4)
write.csv(all_mods, "all_modelsAIC.csv")


means1<-as.data.frame(TripDurBmeans)
means2<-as.data.frame(TripPathBmeans)
means3<-as.data.frame(MaxDistB)
means4<-as.data.frame(sinmeans)

means1$var<-"TripDur"
means2$var<-"TripPath"
means3$var<-"MaxDist"
means4$var<-"sinuos"

all_means<-rbind(means1, means2, means3, means4)
write.csv(all_means, "all_means.csv")


backup<-glmm_tests
#summary ALL TRIPS with PC1 and sinuosity 

#same file called earlier... to fix this in the workflow 
glmm_testsPCA<-readRDS("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_testsPCA.RDS")
#saveRDS(glmm_testsPCA, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/glmm_testsPCA.RDS")
glmm_testsPCA$Year <- as.factor(glmm_testsPCA$Year)


glmm_tests<-glmm_testsPCA
#PCA models
options(na.action = "na.omit")
glmm_PCAeffort <- lme4::lmer(PC1 ~
                               log(sinuos)*Year*Spec +
                               (1 | dep_id),
                             REML = FALSE,  
                             data = glmm_tests)

summary(glmm_PCAeffort)
plot(glmm_PCAeffort)
plot(resid(glmm_PCAeffort),(glmm_tests$PC1))
lattice::qqmath(glmm_PCAeffort, id=0.05)
boxplot(resid(glmm_PCAeffort)~ interaction(glmm_tests$Year, glmm_tests$Spec))

car::leveneTest(resid(glmm_PCAeffort), interaction(glmm_tests$Year, glmm_tests$Spec))


options(na.action = "na.omit")
glmm_PCAeffort <- nlme::lme( PC1 ~ 
                               log(sinuos)*Year*Spec , 
                             random = ~1|dep_id, 
                             method = "ML",
                             weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                             data = glmm_tests)

summary(glmm_PCAeffort)
plot(glmm_PCAeffort)
plot(resid(glmm_PCAeffort),(glmm_tests$PC1))
qqnorm(glmm_PCAeffort, ~ residuals(., type = "p"), abline = c(0, 1))

car::leveneTest(residuals(glmm_PCAeffort, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))

boxplot(residuals(glmm_PCAeffort, type = "pearson") ~ interaction(glmm_tests$Year, glmm_tests$Spec))
options(na.action = "na.fail")

aic_effortPCA<-MuMIn::dredge(glmm_PCAeffort)
aic_effortPCA


options(na.action = "na.omit")
glmm_PCAeffortB <- nlme::lme( PC1 ~ 
                                log(sinuos)*Year+
                                Spec*Year, 
                              random = ~1|dep_id, 
                              method = "REML",
                              weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                              data = glmm_tests[order(glmm_tests$Spec),])

summary(glmm_PCAeffortB)
plot(glmm_PCAeffortB)
plot(residuals(glmm_PCAeffortB, type = "pearson"), (glmm_tests$PC1))
qqnorm(glmm_PCAeffortB, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(glmm_PCAeffortB, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

car::leveneTest(residuals(glmm_PCAeffortB, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))


sjPlot::plot_model(glmm_PCAeffortB, type = "pred", terms = c("sinuos [exp]","Year","Spec"),
                   #axis.lim = list(c(0.02,0.06),c(-3,3))
                   title = "",
                   axis.labels = "Foraging Effort (PC1)" ,#c("Foraging Effort (PC1)", "Sinuosity (log)")
                   #se=TRUE,
                   #pred.type = "re"
)

#no data (check why the added data plot makes the curves the SAME!!!!!!!!!!!!!)
ggplot()+
  geom_smooth(data=ggeffects::ggpredict(
    glmm_PCAeffortB,
    terms = c("sinuos","Year","Spec"),
    ci.lvl = 0.95,
    type = "fe", 
    typical = "mean",
    back.transform = TRUE,
    interval = "confidence"),
    aes(x = (x), y = (predicted), color = group),
    method = "lm"
  )+
  xlab("Sinuosity (log)")+
  ylab("Foraging effort (PC1)")+
  facet_wrap("facet")+
  guides( linetype= 'none')+
  labs(color='Year')+ 
  #labs(title = "Foraging Effort in Guano Seabirds")+
  theme_bw()

#make a save

meanModPCAsinu<-ggeffects::ggpredict(
  glmm_PCAeffortB,
  terms = c("sinuos","Year","Spec"),
  ci.lvl = 0.95,
  type = "fe"
  ,  typical = "mean"
)

#save aic table for PC1 effort year spec and sinous
aic_effortPCA.df <- as.data.frame(aic_effortPCA)

aic_effortPCA.df$model<-"aic_effortPCA"

#write.csv(aic_effortPCA.df, "aic_effortPCA.csv")

###NOT NEEDED############################################################################################################

ggeffects::ggpredict(glmm_PCAeffortB, terms = c("Year","Spec"), type = "re")

#ggpredict(xxx, terms = c("Year","Spec"), type = "re")

# Foraging effort plot models species and years
##wiTH Added points as Kyle suggested! 
forEffort_plot <-ggplot()+
  geom_point(data= glmm_tests, aes(x=(sinuos), y=PC1), alpha = 0.3)+
  geom_smooth(data=ggeffects::ggpredict(
    glmm_PCAeffortB,
    terms = c("sinuos","Year","Spec"),
    ci.lvl = 0.95,
    type = "fe", 
    typical = "mean",
    back.transform = FALSE,
    interval = "confidence"),
    aes(x = (x), y = (predicted), color = group),
    method = "lm",
    fullrange=TRUE
  )+
  xlab("Sinuosity (log)")+
  ylab("Foraging effort (PC1)")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  facet_wrap("Spec")+
  guides( linetype= 'none')+
  labs(color='Year')+ 
  theme_bw()

forEffort_plot
ggplot2::ggsave("plots/forEffort_plotModel.png", forEffort_plot, dpi = 300, bg = "white", units = 'in', 
                width = 8, height = 4.5)


plot(ggeffects::ggpredict(glmm_PCAeffortB, terms = c("Year","Spec"), type = "re",
                          typical = "mean"))

options(na.action = "na.omit")
newMod<-nlme::lme( log(as.numeric(TimeTrip) )~ 
                     log(sinuos)*Year*Spec , 
                   random = ~1|dep_id, 
                   method = "ML",
                   weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                   data = glmm_tests)
summary(newMod)
plot(newMod)
plot(residuals(newMod, type = "pearson"), log(as.numeric(glmm_tests$TimeTrip)))
qqnorm(newMod, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(newMod, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

ggeffects::ggpredict(
  newMod,
  terms = c("sinuos","Year","Spec"),
  ci.lvl = 0.95,
  type = "e",
  typical = "mean")

sjPlot::plot_model(newMod, type = "pred", terms = c("sinuos","Year","Spec"), back.transform = FALSE)

###
###

##
##Tests for relation between time of day of sampling and metabolites
par(mfrow = c(2, 2))

glmm_tests <- glmm_tests|>
  dplyr::dplyr::filter(!grepl('RLCO', dep_id) )|> 
  dplyr::mutate(
    timeDAY = lubridate::hour(timeR) + (lubridate::minute(timeR) + lubridate::second(timeR)/60)/60,
    hour = lubridate::hour(timeR)
  )

#glu

glmm_testsglu <- glmm_tests |>
  dplyr::filter(!is.na(glu))|>
  dplyr::filter(!(Year == 2018))

par(mfrow = c(2, 2))

summary(lm(log(glmm_testsglu$glu)~ log(glmm_testsglu$timeDAY)))
datesglutrans <- lm(log(glmm_testsglu$glu)~ log(glmm_testsglu$timeDAY))
plot(datesglutrans)
par(mfrow = c(1, 1))
plot(log(glmm_testsglu$glu)~ log(glmm_testsglu$timeDAY))
abline(lm(log(glmm_testsglu$glu)~ log(glmm_testsglu$timeDAY)))
sqrt(summary(datesglutrans)$r.squared)
sqrt(summary(datesglutrans)$r.squared)

par(mfrow = c(2, 2))
datesglu<-lm((glmm_testsglu$glu)~ (glmm_testsglu$timeDAY))
summary(lm((glmm_testsglu$glu)~ (glmm_testsglu$timeDAY)))
plot(datesglu)
par(mfrow = c(1, 1))

plot((glmm_testsglu$glu)~ (glmm_testsglu$timeDAY))
abline(lm((glmm_testsglu$glu)~ (glmm_testsglu$timeDAY)))


sqrt(summary(datesglu)$r.squared)
sqrt(summary(datesglu)$r.squared)

#CHOL
glmm_testschol <- glmm_tests |>
  dplyr::filter(!is.na(chol))|>
  dplyr::filter(!(Year == 2018))

par(mfrow = c(2, 2))

summary(lm(log(glmm_testschol$chol)~ log(glmm_testschol$timeDAY)))
datescholtrans <- lm(log(glmm_testschol$chol)~ log(glmm_testschol$timeDAY))
plot(datescholtrans)
par(mfrow = c(1, 1))
plot(log(glmm_testschol$chol)~ log(glmm_testschol$timeDAY))
abline(lm(log(glmm_testschol$chol)~ log(glmm_testschol$timeDAY)))
sqrt(summary(datescholtrans)$r.squared)
sqrt(summary(datescholtrans)$r.squared)

par(mfrow = c(2, 2))
dateschol<-lm((glmm_testschol$chol)~ (glmm_testschol$timeDAY))
summary(lm((glmm_testschol$chol)~ (glmm_testschol$timeDAY)))
plot(dateschol)
par(mfrow = c(1, 1))

plot((glmm_testschol$chol)~ (glmm_testschol$timeDAY))
abline(lm((glmm_testschol$chol)~ (glmm_testschol$timeDAY)))

sqrt(summary(dateschol)$r.squared)
sqrt(summary(dateschol)$r.squared)

#tri

glmm_teststri <- glmm_tests |>
  dplyr::filter(!is.na(tri))|>
  dplyr::filter(!(Year == 2018))

par(mfrow = c(2, 2))

summary(lm(log(glmm_tests$tri)~ log(glmm_tests$timeDAY)))
datestritrans <- lm(log(glmm_tests$tri)~ log(glmm_tests$timeDAY))
plot(datestritrans)
par(mfrow = c(1, 1))
plot(log(glmm_tests$tri)~ log(glmm_tests$timeDAY))
abline(lm(log(glmm_tests$tri)~ log(glmm_tests$timeDAY)))
sqrt(summary(datestritrans)$r.squared)
sqrt(summary(datestritrans)$r.squared)

par(mfrow = c(2, 2))
datestri<-lm((glmm_tests$tri)~ (glmm_tests$timeDAY))
summary(lm((glmm_tests$tri)~ (glmm_tests$timeDAY)))
plot(datestri)
par(mfrow = c(1, 1))

plot((glmm_tests$tri)~ (glmm_tests$timeDAY))
abline(lm((glmm_tests$tri)~ (glmm_tests$timeDAY)))

sqrt(summary(datestri)$r.squared)
sqrt(summary(datestri)$r.squared)

#ket


glmm_testsket <- glmm_tests |>
  dplyr::filter(!is.na(ket))|> 
  dplyr::filter(!(Year == 2018))
par(mfrow = c(2, 2))
summary(lm(log(glmm_testsket$ket)~ log(glmm_testsket$timeDAY)))
dateskettrans <- lm(log(glmm_testsket$ket)~ log(glmm_testsket$timeDAY))
plot(dateskettrans)
par(mfrow = c(1, 1))
plot(log(glmm_testsket$ket)~ log(glmm_testsket$timeDAY))
abline(lm(log(glmm_testsket$ket)~ log(glmm_testsket$timeDAY)))
sqrt(summary(dateskettrans)$r.squared)
sqrt(summary(dateskettrans)$r.squared)


par(mfrow = c(2, 2))
datesket<-lm((glmm_testsket$ket)~ (glmm_testsket$timeDAY))
summary(lm((glmm_testsket$ket)~ (glmm_testsket$timeDAY)))
plot(datesket)
par(mfrow = c(1, 1))

plot((glmm_testsket$ket)~ (glmm_testsket$timeDAY))
abline(lm((glmm_testsket$ket)~ (glmm_testsket$timeDAY)))

sqrt(summary(datesket)$r.squared)
sqrt(summary(datesket)$r.squared)


#
###
###
###

#Test for DIFFERENCES BETWEEN SPECIES boobies and cormorants in 2019

#GLU
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(glu))
glumod<-lm(data=phys, log(glu)~ SP)

summary(glumod)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(glumod)
par(opar)

car::leveneTest(resid(glumod), interaction(phys$Year, phys$SP))
boxplot(resid(glumod) ~ interaction(phys$Year, phys$SP))

glumod0 <- lm(data=phys, log(glu)~ 1)

options(na.action = "na.fail")
aic_lmGluSP<-MuMIn::model.sel(glumod0, glumod)

aic_lmGluSP


glumod2019<-ggeffects::ggemmeans(model = glumod, 
                                 terms = c('SP'),
                                 ci.lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back.transform = FALSE,
                                 interval = "confidence")

#plots 
glu2<-ggplot(data= phys, aes(x=SP, y=log(glu), color = SP))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = glumod, 
                                            terms = c('SP'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Glucose (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(text= ggplot2::element_text(size=20),
        axis.text.x=element_blank())
glu2


#chol

phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(chol))
cholmod<-lm(data=phys, log(chol)~ SP)

summary(cholmod)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(glumod)
par(opar)

car::leveneTest(resid(cholmod), interaction(phys$Year, phys$SP))
boxplot(resid(cholmod) ~ interaction(phys$Year, phys$SP))



cholmod0 <- lm(data=phys, log(chol)~ 1)

options(na.action = "na.fail")
aic_lmCholSP<-MuMIn::model.sel(cholmod0, cholmod)

aic_lmCholSP

cholmod2019<-ggeffects::ggemmeans(model = cholmod, 
                                  terms = c('SP'),
                                  ci.lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back.transform = FALSE,
                                  interval = "confidence")

#plots 
chol2<-ggplot(data= phys, aes(x=SP, y=log(chol), color = SP))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = cholmod, 
                                            terms = c('SP'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Cholesterol (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(text= ggplot2::element_text(size=20),
        axis.text.x=element_blank())

chol2

#tri

phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(tri))

trimod<-lm(data=phys, log(tri)~ SP)

summary(cholmod)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(trimod)
par(opar)

car::leveneTest(resid(trimod), interaction(phys$Year, phys$SP))
boxplot(resid(trimod) ~ interaction(phys$Year, phys$SP))


trimod <- nlme::lme( log(tri)~ SP, 
                     random = ~1|DepID, 
                     method = "REML",
                     weights = nlme::varIdent(form = ~ 1 | SP),
                     data = phys[order(phys$SP),])

car::leveneTest(residuals(trimod, type = "pearson"), interaction(phys$SP))
boxplot(residuals(trimod, type = "pearson") ~ interaction(phys$SP))

summary(trimod)

trimod0 <- nlme::lme( log(tri)~ 1, 
                      random = ~1|DepID, 
                      method = "REML",
                      weights = nlme::varIdent(form = ~ 1 | SP),
                      data = phys[order(phys$SP),])

options(na.action = "na.fail")
aic_lmTriSP<-MuMIn::model.sel(trimod0, trimod)

aic_lmTriSP


trimod2019<-ggeffects::ggemmeans(model = trimod, 
                                 terms = c('SP'),
                                 ci.lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back.transform = FALSE,
                                 interval = "confidence")

# 

tri2<-ggplot(data= phys, aes(x=SP, y=log(tri), color = SP))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = trimod, 
                                            terms = c('SP'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Triglycerides (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(
    text= ggplot2::element_text(size=20)
  )

tri2

#ket
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(ket))
ketmod<-lm(data=phys, log(ket)~ SP)

summary(ketmod)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ketmod)
par(opar)

car::leveneTest(resid(ketmod), interaction(phys$Year, phys$SP))
boxplot(resid(ketmod) ~ interaction(phys$Year, phys$SP))

ketmod0<-lm(data=phys, log(ket)~ 1)

options(na.action = "na.fail")
aic_lmKetSP<-MuMIn::model.sel(ketmod, ketmod0)

aic_lmKetSP

ketmod2019<-ggeffects::ggemmeans(model = ketmod, 
                                 terms = c('SP'),
                                 ci.lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back.transform = FALSE,
                                 interval = "confidence")

#plots 
ket2<-ggplot(data= phys, aes(x=SP, y=log(ket), color = SP))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = ketmod, 
                                            terms = c('SP'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Ketones (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(
    text= ggplot2::element_text(size=20)
  )
ket2

metabs2019<-cowplot::plot_grid(glu2, chol2, tri2, ket2, labels = c('A', 'B', 'C', 'D'), ncol = 2)
metabs2019
ggplot2::ggsave("plots/metabs2019_plot.png", metabs2019, dpi = 300, bg = "white", units = 'in', 
                width = 12, height = 8.5)

#models for 2 species 2019 mets AICc's
mod1<- as.data.frame(aic_lmGluSP)
mod2<-as.data.frame(aic_lmCholSP)
mod3<-as.data.frame(aic_lmTriSP)
mod4<-as.data.frame(aic_lmKetSP)

mod1$model<-"aic_lmGluSP"
mod2$model<-"aic_lmCholSP"
mod3$model<-"aic_lmTriSP"
mod4$model<-"aic_lmKetSP"

all_mods<-rbind(mod1, mod2, mod3, mod4)
#write.csv(all_mods, "metsSpecies_modelsAIC.csv")


metmeans1<-as.data.frame(glumod2019)
metmeans2<-as.data.frame(cholmod2019)
metmeans3<-as.data.frame(trimod2019)
metmeans4<-as.data.frame(ketmod2019)

metmeans1$var<-"glu"
metmeans2$var<-"chol"
metmeans3$var<-"tri"
metmeans4$var<-"ket"

met_all_means<-rbind(metmeans1,metmeans2,metmeans3,metmeans4)
#write.csv(met_all_means, "met_all_means.csv")


# Test for DIFFERENCES IN BOOBIES in 2019 and 2018
#GLU
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(glu))
glumodP<-lm(data=phys, log(glu)~ Year)

summary(glumodP)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(glumodP)
par(opar)

car::leveneTest(resid(glumodP), interaction(phys$Year))
boxplot(resid(glumodP) ~ interaction(phys$Year))


glumodP <- nlme::lme( log(glu)~ Year, 
                      random = ~1|DepID, 
                      method = "REML",
                      weights = nlme::varIdent(form = ~ 1 | Year),
                      data = phys[order(phys$Year),])
car::leveneTest(residuals(glumodP, type = "pearson"), interaction(phys$Year))
boxplot(residuals(glumodP, type = "pearson") ~ interaction(phys$Year))

summary(glumodP)

glumodP0 <- nlme::lme( log(glu)~ 1, 
                       random = ~1|DepID, 
                       method = "REML",
                       weights = nlme::varIdent(form = ~ 1 | Year),
                       data = phys[order(phys$Year),])

options(na.action = "na.fail")

aic_lmGLUPeb<-MuMIn::model.sel(glumodP, glumodP0)

aic_lmGLUPeb


meanglumodP<-ggeffects::ggemmeans(model = glumodP, 
                                  terms = c('Year'),
                                  ci.lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back.transform = FALSE,
                                  interval = "confidence")

#plots 
gluPEBO<-ggplot(data= phys, aes(x=Year, y=log(glu), color = Year))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = glumodP, 
                                            terms = c('Year'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Glucose (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(axis.text.x=element_blank())
gluPEBO


#chol
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(chol))

cholmodP<-lm(data=phys, log(chol)~ Year)

summary(cholmodP)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(cholmodP)
par(opar)

car::leveneTest(resid(cholmodP), interaction(phys$Year))
boxplot(resid(cholmodP) ~ interaction(phys$Year))


###ACCOUNTING FOR DIFF VARIANCES
cholmodP <- nlme::lme( log(chol)~ Year, 
                       random = ~1|DepID, 
                       method = "ML",
                       weights = nlme::varIdent(form = ~ 1 | Year),
                       data = phys[order(phys$Year),])

car::leveneTest(residuals(cholmodP, type = "pearson"), interaction(phys$Year))

boxplot(residuals(cholmodP, type = "pearson") ~ interaction(phys$Year))

summary(cholmodP)

cholmodP0 <- nlme::lme( log(chol)~ 1, 
                        random = ~1|DepID, 
                        method = "ML",
                        weights = nlme::varIdent(form = ~ 1 | Year),
                        data = phys[order(phys$Year),])

options(na.action = "na.fail")
aic_lmCHOLPe<-MuMIn::model.sel(cholmodP, cholmodP0)

aic_lmCHOLPe


meancholmodP<-ggeffects::ggemmeans(model = cholmodP, 
                                   terms = c('Year'),
                                   ci.lvl = 0.95,
                                   type = "fe",
                                   typical = "mean",
                                   condition = NULL,
                                   back.transform = FALSE,
                                   interval = "confidence")

#plots 

cholPEBO<-ggplot(data= phys, aes(x=Year, y=log(chol), color = Year))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = cholmodP, 
                                            terms = c('Year'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Cholesterol (log)")+
  guides(color = "none")+
  theme_bw()+
  theme(axis.text.x=element_blank())

cholPEBO


#tri
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(tri))

trimodP<-lm(data=phys, log(tri)~ Year)

summary(trimodP)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(cholmodP)
par(opar)

car::leveneTest(resid(trimodP), interaction(phys$Year))
boxplot(resid(trimodP) ~ interaction(phys$Year))

trimodP0<-lm(data=phys, log(tri)~ 1)


options(na.action = "na.fail")
aic_lmTriPe<-MuMIn::model.sel(trimodP, trimodP0)

aic_lmTriPe


meantrimodP<-ggeffects::ggemmeans(model = trimodP, 
                                  terms = c('Year'),
                                  ci.lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back.transform = FALSE,
                                  interval = "confidence")

#plots 
#TIMETRIP YEAR AND SPECIES
triPEBO<-ggplot(data= phys, aes(x=Year, y=log(tri), color = Year))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = trimodP, 
                                            terms = c('Year'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Triglycerides (log)")+
  guides(color = "none")+
  theme_bw()

triPEBO

#ket
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::filter(!is.na(ket))

ketmodP<-lm(data=phys, log(ket)~ Year)

summary(ketmodP)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ketmodP)
par(opar)

car::leveneTest(resid(ketmodP), interaction(phys$Year))
boxplot(resid(ketmodP) ~ interaction(phys$Year))


summary(ketmodP)

ketmodP <- nlme::lme( log(ket)~ Year, 
                      random = ~1|DepID, 
                      method = "REML",
                      weights = nlme::varIdent(form = ~ 1 | Year),
                      data = phys[order(phys$Year),])
car::leveneTest(residuals(ketmodP, type = "pearson"), interaction(phys$Year))
boxplot(residuals(ketmodP, type = "pearson") ~ interaction(phys$Year))




ketmodP0 <- nlme::lme( log(ket)~ 1, 
                       random = ~1|DepID, 
                       method = "REML",
                       weights = nlme::varIdent(form = ~ 1 | Year),
                       data = phys[order(phys$Year),])


options(na.action = "na.fail")
aic_lmKetPe<-MuMIn::model.sel(ketmodP, ketmodP0)

aic_lmKetPe

meanketmodP<-ggeffects::ggemmeans(model = ketmodP, 
                                  terms = c('Year'),
                                  ci.lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back.transform = FALSE,
                                  interval = "confidence")

#plots 

ketPEBO<-ggplot(data= phys, aes(x=Year, y=log(ket), color = Year))+ #, color = Spec
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggemmeans(model = ketmodP, 
                                            terms = c('Year'),
                                            ci.lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back.transform = FALSE,
                                            condition = NULL) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab(NULL)+
  ylab("Ketones (log)")+
  guides(color = "none")+
  theme_bw()
ketPEBO


metabPEBO<-cowplot::plot_grid(gluPEBO, cholPEBO, triPEBO, ketPEBO, labels = c('A', 'B', 'C', 'D'), ncol = 2)
metabPEBO

ggplot2::ggsave("plots/metabsPEBO2018_19.png", metabPEBO , dpi = 300, bg = "white", units = 'in', 
                width = 8, height = 5.5)

#models AICs for Boobies  2018 2019 mets 


mod1<- as.data.frame(aic_lmGLUPeb)
mod2<-as.data.frame(aic_lmCHOLPe)
mod3<-as.data.frame(aic_lmTriPe)
mod4<-as.data.frame(aic_lmKetPe)

mod1$model<-"aic_lmGLUPeb"
mod2$model<-"aic_lmCHOLPe"
mod3$model<-"aic_lmTriPe"
mod4$model<-"aic_lmKetPe"

all_mods<-rbind(mod1, mod2, mod3, mod4)
#write.csv(all_mods, "metsPEBO2018_19_modelsAIC.csv")


metmeans1P<-as.data.frame(meanglumodP)
metmeans2P<-as.data.frame(meancholmodP)
metmeans3P<-as.data.frame(meantrimodP)
metmeans4P<-as.data.frame(meanketmodP)

metmeans1P$var<-"glu"
metmeans2P$var<-"chol"
metmeans3P$var<-"tri"
metmeans4P$var<-"ket"

met_all_meansP<-rbind(metmeans1P,metmeans2P,metmeans3P,metmeans4P)
#write.csv(met_all_meansP, "met_all_meansPEBO.csv")


#########################################################################################################################################
#PCA of metabolites: NOT USED
complete.cases() all_deps[, c("glu","chol","tri", "ket")]

all_deps1<-all_deps|>
  dplyr::filter(!is.na(glu))|>
  dplyr::filter(!is.na(tri))|>
  dplyr::filter(!is.na(chol))|>
  dplyr::filter(!is.na(ket)) 

res.pca <- prcomp(all_deps1[, c("glu","chol","tri", "ket")], scale = TRUE)  

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

groups <- as.factor(all_deps1$SP)
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
             habillage = all_deps1$SP, # color by groups
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


