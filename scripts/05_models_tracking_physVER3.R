#models for tracking and phys
#edit on Nov 2024
#now has been run with interpolated data
# reported results plots and tables are v2
# metabolites stand alone analysese, remained the same



################################################################
################################################################
#Models for hypothesis 1: foraging effort and years, sp, and sex
################################################################
################################################################


#load and process summaries for trips
{#original deployment csvs fused into 1 (2018, and 2019 gps, cams, and axxy)
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
    maxdist = maxdist/1000,
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
}

# check all, then remove long trips, in tiime first, and in distance after, and then both
ggpubr::ggscatter(glmm_tests#[which( glmm_tests$tottime <8 & glmm_tests$maxdist <60),]
                  , 
                  y = "tottime", 
                  x = "TimeTrip",
                  color = "black", shape = 21, #size = 3, # Points color, shape and size
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                  cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                        label.sep = "\n"),
                  title = "check for time metric"
)


#ggpairs(glmm_tests[,c("TimeTrip", "maxdist", "totdist","sinuos", "SP")], ggplot2::aes(colour = SP), progress = FALSE) +
# ggplot2::theme_bw()

#obtain mean duration of trip value per year (median to avoid a couple of extremes)
#for Axxy paper to estimate energy away with time budgets

#per bird mean metrics (including all trips performed in multipe days), not super accurate descriptor
glmm_tests|>
  dplyr::group_by(ID, Day, Year, Spec)|>
  dplyr::summarise(
    total_Trip = sum(tottime),
    tota_trav = sum (totdist)/1000
  )|>
  dplyr::ungroup()|>
  dplyr::group_by(Year, Spec)|>
  dplyr::summarise(
    meanAway = median(total_Trip),
    meanDist = median(tota_trav)
  )

#per trip mean metrics per species and year
sum_table_4metrics<-glmm_tests|>
 
  dplyr::group_by(Year, Spec)|>
  dplyr::summarise(
    meanAway = mean(tottime),
    SEmeanAway =  sd(tottime)/sqrt(dplyr::n()),
    minAway = min(tottime, na.rm = TRUE),
    maxAway = max(tottime, na.rm = TRUE),
    
    meanDist = mean(totdist/1000),
    SEmeanDist = (sd(totdist/1000))/sqrt(dplyr::n()),
    minDist = min(totdist/1000, na.rm = TRUE),
    maxDist = max(totdist/1000, na.rm = TRUE),
    
    meanMax = mean(maxdist),
    SEmeanMax = sd(maxdist)/sqrt(dplyr::n()), 
    minMax = min(maxdist, na.rm = TRUE),
    maxMax = max(maxdist, na.rm = TRUE),
    
    meanSin = mean(sinuos/1000),
    SEmeanSin = sd(sinuos/1000)/sqrt(dplyr::n()),
    minSin = min(sinuos, na.rm = TRUE),
    maxSin = max(sinuos, na.rm = TRUE)
  )
write.csv(sum_table_4metrics, "data/sum_table_4metrics.csv")
#per trip mean metrics per species and year, independently
#YEAR
glmm_tests|>
  dplyr::group_by(Year)|>
  dplyr::summarise(
    meanAway = mean(tottime),
    SEmeanAway =  sd(tottime)/sqrt(dplyr::n()),
    meanDist = mean(totdist/1000),
    SEmeanDist = (sd(totdist/1000))/sqrt(dplyr::n()),
    meanMax = mean(maxdist),
    SEmeanMax = sd(maxdist)/sqrt(dplyr::n()), 
    meanSpeed = mean(MaxSpeed/1000),
    SEmeanSpeed = sd(MaxSpeed/1000)/sqrt(dplyr::n())
    )
#Species
sample_means <-glmm_tests|>
  dplyr::group_by(Spec)|>
  dplyr::summarise(
    meanAway = mean(tottime),
    SEmeanAway =  sd(tottime)/sqrt(dplyr::n()),
    meanDist = mean(totdist/1000),
    SEmeanDist = (sd(totdist/1000))/sqrt(dplyr::n()),
    meanMax = mean(maxdist),
    SEmeanMax = sd(maxdist)/sqrt(dplyr::n()), 
    meanSpeed = mean(MaxSpeed/1000),
    SEmeanSpeed = sd(MaxSpeed/1000)/sqrt(dplyr::n())
  )
write.csv(sample_means , "data/sample_means .csv")

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
options(na.action = "na.omit")
glmm_TripDur <- nlme::lme( log(TimeTrip) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|ID, 
                           method = "ML",
                           #weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                           data = glmm_tests)

summary(glmm_TripDur)
plot(glmm_TripDur)
plot(resid(glmm_TripDur),log(glmm_tests$TimeTrip))
qqnorm(glmm_TripDur, ~ residuals(., type = "p"), abline = c(0, 1))

car::leveneTest(residuals(glmm_TripDur, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))

boxplot(residuals(glmm_TripDur, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))



glmm_TripDur <- nlme::lme( log(TimeTrip) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|ID, 
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
                            random = ~1|ID, 
                            method = "REML",
                           weights = nlme::varIdent(form = ~ 1 | Year),
                            data = glmm_tests[order(glmm_tests$Spec),])


summary(glmm_TripDurB)

plot(glmm_TripDurB)
plot(residuals(glmm_TripDurB, type = "pearson"), log(glmm_tests$TimeTrip))
qqnorm(glmm_TripDurB, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(glmm_TripDurB, type = "pearson")~ interaction(glmm_tests$Year))

car::leveneTest(residuals(glmm_TripDurB, type = "pearson"), interaction(glmm_tests$Year))


TripDurBmeans<-ggeffects::ggemmeans(model = glmm_TripDurB, 
                                    terms = c('Year'),
                                    ci_lvl = 0.95,
                                    type = "fe",
                                    typical = "mean",
                                    condition = NULL,
                                    back_transform = FALSE,
                                    interval = "confidence")

#plots 
#TIMETRIP YEAR AND SPECIES

A2<-ggplot2::ggplot(data= glmm_tests,ggplot2::aes(x=Year, y=log(TimeTrip), color = Spec))+ #, linetype = Year
  ggplot2::geom_point( ggplot2::aes( color = Spec),
                       position = ggplot2::position_jitterdodge(dodge.width = 0.3, 
                                                                jitter.height = 0,
                                                                jitter.width = 0.3
                       ),
                       alpha = 0.3)+
  ggplot2::geom_pointrange(data=TripDurBmeans,
                           ggplot2::aes(x = x, y = predicted, 
                                        ymin = predicted-std.error, ymax = predicted+std.error,
                                        color = x) )+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Total trip duration (h)")+
  ggplot2::guides(color = "none")+
  ggplot2::scale_colour_manual(values = c("#F8766D",  "#00BFC4","magenta4", "darkgreen"))+
  ggplot2::theme_bw()
A2

###########################################################################
#TOTAL DISTANCE trip path
options(na.action = "na.omit")

glmm_TripPath <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|ID, 
                           method = "ML",
                           #weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                           data = glmm_tests)
plot(glmm_TripPath)
plot(residuals(glmm_TripPath, type = "pearson"), log(glmm_tests$totdist))
qqnorm(glmm_TripPath, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(glmm_TripPath, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))
car::leveneTest(resid(glmm_TripPath, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))


glmm_TripPath <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                              Year*Spec, 
                            random = ~1|ID, 
                            method = "ML",
                            weights =  nlme::varIdent(form = ~ 1 | Year*Spec),
                            data = glmm_tests)
summary(glmm_TripPath)
plot(glmm_TripPath)
plot(residuals(glmm_TripPath, type = "pearson"), log(glmm_tests$TimeTrip))
qqnorm(glmm_TripPath, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(residuals(glmm_TripPath, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

car::leveneTest(residuals(glmm_TripPath, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
# doesnt really improve with weights
# just Spec is fine

options(na.action = "na.fail")
aic_TripPath<-MuMIn::dredge(glmm_TripPath)
aic_TripPath






#just  Species effect, as it is the most parsimoneus model (within 2 dAIC )
glmm_TripPathB <- nlme::lme( log(totdist) ~ #log_TotDist totdist
                               as.factor(Spec), 
                             random = ~1|ID, 
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
                                     ci_lvl = 0.95,
                                     type = "fe",
                                     typical = "mean",
                                     condition = NULL,
                                     back_transform = FALSE,
                                     interval = "confidence")



B2<-ggplot2::ggplot(data= glmm_tests,ggplot2::aes(x=Spec, y=log(totdist), color = Year))+ #, linetype = Year
  ggplot2::geom_point( ggplot2::aes( color = Year),
                       position = ggplot2::position_jitterdodge(dodge.width = 0.3, 
                                                                jitter.height = 0,
                                                                jitter.width = 0.3
                       ),
                       alpha = 0.3)+
  # ggplot2::scale_colour_manual(values = c("#F8766D",  "#00BFC4"))+
  
  #ggnewscale::new_scale_color()+
  ggplot2::geom_pointrange(data=TripPathBmeans,
                           ggplot2::aes(x = x, y = predicted, 
                                        ymin = predicted-std.error, ymax = predicted+std.error,
                                        color = x) )+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Total trip path (km)")+
  ggplot2::guides(color = "none")+
  ggplot2::scale_colour_manual(values = c("magenta4", "darkgreen", "#F8766D",  "#00BFC4" ))+
  ggplot2::theme_bw()
B2

##############################################################
#MAX DISTANCE
options(na.action = "na.omit")

glmm_MaxDist <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                              Year*Spec, 
                            random = ~1|ID, 
                            method = "ML",
                            #weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                            data = glmm_tests)
plot(glmm_MaxDist)
plot(residuals(glmm_MaxDist, type = "pearson"), log(glmm_tests$maxdist))
qqnorm(glmm_MaxDist, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(resid(glmm_MaxDist)~ interaction(glmm_tests$Year, glmm_tests$Spec))
car::leveneTest(resid(glmm_MaxDist), interaction(glmm_tests$Year, glmm_tests$Spec))

glmm_MaxDist <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|ID, 
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

#NULL IS WITHIN 2 delta AIC


#best parsim model for max dist
glmm_MaxDistB <- nlme::lme( log(maxdist) ~ #log_TotDist totdist
                              Spec, 
                            random = ~1|ID, 
                            method = "REML",
                            weights = nlme::varIdent(form = ~ 1 | Spec),
                            data = glmm_tests[order(glmm_tests$Spec),])

car::leveneTest(residuals(glmm_MaxDistB, type = "pearson"), interaction(glmm_tests$Spec))
plot(glmm_MaxDistB)
plot(resid(glmm_MaxDistB),log(glmm_tests$maxdist))

boxplot(residuals(glmm_MaxDistB, type= "pearson")~ interaction( glmm_tests$Spec))

MaxDistB<-ggeffects::ggemmeans(model = glmm_MaxDistB, 
                               terms = c('Spec'),
                               ci_lvl = 0.95,
                               type = "fe",
                               typical = "mean",
                               condition = NULL,
                               back_transform = FALSE,
                               interval = "confidence")

C2<-
  ggplot2::ggplot(data= glmm_tests, ggplot2::aes(x=Spec, y=log(maxdist), color = Spec ))+ 
  ggplot2::geom_point(ggplot2::aes( color = Year),
                      position = ggplot2::position_jitterdodge(dodge.width = 0.3, 
                                                               jitter.height = 0,
                                                               jitter.width = 0.3
                      ),
                      alpha = 0.3)+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = glmm_MaxDistB, 
                                                     terms = c('Spec'),
                                                     ci_lvl = 0.95,
                                                     type = "fe",
                                                     typical = "mean",
                                                     condition = NULL,
                                                     back_transform = FALSE) ,
                           ggplot2::aes(x = x, y = predicted, 
                                        ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Max. distance from the colony (km)")+
  ggplot2::guides(color = "none")+
  ggplot2::scale_colour_manual(values = c("magenta4", "darkgreen", "#F8766D",  "#00BFC4" ))+
  
  ggplot2::theme_bw()


C2


#to plot model with year and spec for max dist ,need to work another model and plot



############################################################
#SINUOSITY
options(na.action = "na.omit")

glmm_sin <- nlme::lme( log(sinuos) ~ #log_TotDist totdist
                             Year*Spec, 
                           random = ~1|ID, 
                           method = "ML",
                           #weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                           data = glmm_tests)
plot(glmm_sin)
plot(residuals(glmm_sin, type = "pearson"), log(glmm_tests$sinuos))
qqnorm(glmm_sin, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(resid(glmm_sin, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

car::leveneTest(resid(glmm_sin, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))



glmm_sin <- nlme::lme( log(sinuos) ~ #log_TotDist totdist
                         Year*Spec, 
                       random = ~1|ID, 
                       method = "ML",
                       weights = nlme::varIdent(form = ~ 1 | Year*Spec), #activated after testing without
                       data = glmm_tests)
plot(glmm_sin)
plot(residuals(glmm_sin, type = "pearson"), log(glmm_tests$sinuos))
qqnorm(glmm_sin, ~ residuals(., type = "p"), abline = c(0, 1))
boxplot(resid(glmm_sin, type = "pearson")~ interaction(glmm_tests$Year, glmm_tests$Spec))

car::leveneTest(resid(glmm_sin, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))
# also an issue with this still

options(na.action = "na.fail")
aic_sin<-MuMIn::dredge(glmm_sin)
aic_sin



sinmeans<-ggeffects::ggemmeans(model = glmm_sin, 
                               terms = c('Spec', 'Year'),
                               ci_lvl = 0.95,
                               type = "fe",
                               typical = "mean",
                               condition = NULL,
                               back_transform = FALSE,
                               interval = "confidence")


##
D2 <- ggplot2::ggplot(data= glmm_tests, ggplot2::aes(x=Year, y=log(sinuos), color = Spec))+ #, linetype = Year
  ggplot2::geom_point(position = ggplot2::position_jitterdodge(
    dodge.width = 0.3, 
    jitter.height = 0, 
    jitter.width = 0.3),
             alpha = 0.3)+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = glmm_sin, 
                                            terms = c('Year', 'Spec'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            condition = NULL,
                                            back_transform = FALSE,
                                            interval = "confidence") ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error,
                      color = group), 
                  position = ggplot2::position_jitterdodge(dodge.width = 0.3, jitter.height = 0, jitter.width = 0.1) )+#,, linetype = facet
  #method = "lm"
  
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Sinuosity")+
  ggplot2::guides(color = "none")+
  #labs(title = "Foraging Effort in Guano Seabirds")+
  ggplot2::theme_bw()
D2

pcol2<-cowplot::plot_grid(A2, B2, C2, D2, labels = c('A', 'B', 'C', 'D'), ncol = 2)

ggplot2::ggsave("plots/for_effort_metricsv3splits.png", pcol2, dpi = 300, bg = "white", units = 'in', width = 10.5, height = 7)


mod1<- as.data.frame(aic_TripDur)
mod2<-as.data.frame(aic_TripPath)
mod3<-as.data.frame(aic_MaxDist)
mod4<-as.data.frame(aic_sin)

mod1$model<-"aic_TripDur"
mod2$model<-"aic_TripPath"
mod3$model<-"aic_MaxDist"
mod4$model<-"aic_sin"

all_mods<-rbind(mod1, mod2, mod3, mod4)
write.csv(all_mods, "data/all_modelsAICv2.csv")


means1<-as.data.frame(TripDurBmeans)
means2<-as.data.frame(TripPathBmeans)
means3<-as.data.frame(MaxDistB)
means4<-as.data.frame(sinmeans)

means1$var<-"TripDur"
means2$var<-"TripPath"
means3$var<-"MaxDist"
means4$var<-"sinuos"

all_means<-rbind(means1, means2, means3, means4)
#write.csv(all_means, "data/all_meansv2.csv")


############################################################################
############################################################################
################## Adding PCA models for foraging metrics
############################################################################
############################################################################
############################################################################


backup<-glmm_tests

#glmm_tests<-readRDS("data/glmm_testsPCAv2.RDS")
lme.formula <- nlme::lme.formula # only need to call the function once

glmm_tests<-glmm_tests|>
  dplyr::mutate(
    Year = as.factor(Year),
    Spec = as.factor(Spec),
    )
#PCA models
options(na.action = "na.omit")


glmm_PCAeffort <- nlme::lme( PC1 ~ 
                               (sinuos)*Year*Spec , 
                             random = ~1|ID, 
                             method = "ML",
                            # weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                             data = glmm_tests)

summary(glmm_PCAeffort)
plot(glmm_PCAeffort)
plot(resid(glmm_PCAeffort),(glmm_tests$PC1))
qqnorm(glmm_PCAeffort, ~ residuals(., type = "p"), abline = c(0, 1))

car::leveneTest(residuals(glmm_PCAeffort, type = "pearson"), interaction(glmm_tests$Year, glmm_tests$Spec))

boxplot(residuals(glmm_PCAeffort, type = "pearson") ~ interaction(glmm_tests$Year, glmm_tests$Spec))



options(na.action = "na.omit")
glmm_PCAeffort <- nlme::lme( PC1 ~ 
                               sinuos*Year*Spec , 
                             random = ~1|ID, 
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


### best model , most parsimonous

glmm_PCAeffortBEST <- nlme::lme( PC1 ~ 
                                   (sinuos)+Year + Spec +  sinuos:Year +  sinuos:Spec + Spec:Year, 
                             random = ~1|ID, 
                             method = "ML",
                             weights = nlme::varIdent(form = ~ 1 | Year*Spec),
                             data = glmm_tests)


options(na.action = "na.omit")

pred_glmm_PCAeffortBEST<- ggeffects::ggpredict(
  glmm_PCAeffortBEST,
  terms = c("sinuos","Year","Spec"),
  ci_lvl = 0.95,
  type = "fe", 
  typical = "mean",
  back_transform = TRUE,
  interval = "confidence")

forEffort_plot <- plot(pred_glmm_PCAeffortBEST,show_data = TRUE , colors = c("magenta4", "darkgreen"))+
  ggplot2::ggtitle(NULL)+
  ggplot2::xlab("Sinuosity")+
  ggplot2::ylab("Foraging effort (PC1)")

#save aic table for PC1 effort year spec and sinous
aic_effortPCA.df <- as.data.frame(aic_effortPCA)

aic_effortPCA.df$model<-"aic_effortPCA"

#write.csv(aic_effortPCA.df, "data/aic_effortPCAv2_NEWnologSinu.csv")

###plot with points for pub ######################################################

ggeffects::ggpredict(glmm_PCAeffort, terms = c("Year","Spec"), type = "re")

#ggpredict(xxx, terms = c("Year","Spec"), type = "re")


## old version for plotting
# Foraging effort plot models species and years
##wiTH Added points as Kyle suggested! 
#forEffort_plot <- ggplot2::ggplot()+
 # ggplot2::geom_point(data= glmm_tests, ggplot2::aes(x=(sinuos), y=PC1), alpha = 0.3)+
  #ggplot2::geom_smooth(data=ggeffects::ggpredict(
   # glmm_PCAeffort,
    #terms = c("sinuos","Year","Spec"),
    #ci_lvl = 0.95,
    #type = "fe", 
    #typical = "mean",
    #back_transform = FALSE,
    #interval = "confidence"),
    #ggplot2::aes(x = (x), y = (predicted), color = group),
    #method = "lm",
    #fullrange=TRUE
  #)+
  #ggplot2::xlab("Sinuosity (log)")+
  #ggplot2::ylab("Foraging effort (PC1)")+
#  #labs(title = "Foraging Effort in Guano Seabirds")+
 # ggplot2::facet_wrap("Spec")+
  #ggplot2::guides( linetype= 'none')+
  #ggplot2::labs(color='Year')+ 
  #ggplot2::theme_bw()


ggplot2::ggsave("plots/forEffort_plotModelv3.png", forEffort_plot, dpi = 300, bg = "white", units = 'in', 
                width = 8, height = 4.5)



## if only testing 1 metric alone and not PCA1
options(na.action = "na.omit")
newMod<-nlme::lme( log( totdist )~ 
                     log(sinuos)*Year*Spec , 
                   random = ~1|ID, 
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
  ci_lvl = 0.95,
  type = "re",
  typical = "mean")

ggeffects::ggpredict(newMod, terms = c("Year","Spec", "sinuos"), type = "re"
                     )


sjPlot::plot_model(newMod, type = "pred", terms = c("sinuos","Year","Spec"), back_transform = FALSE)

###
###


################################################################
################################################################
### Testing hypothesis of differences in nutrients 
################################################################
################################################################

### Loading glmm_test for metrics and merge with deployment/nutrient data




##
## Tests for relation between TIME OF DAY of sampling and metabolites
par(mfrow = c(2, 2))

glmm_tests <- glmm_tests|>
  dplyr::filter(!grepl('RLCO', ID) )|> 
  dplyr::mutate(
    timeDAY = lubridate::hour(endt) + (lubridate::minute(endt) + lubridate::second(endt)/60)/60,
    hour = lubridate::hour(endt)
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

summary(lm(log(glmm_teststri$tri)~ log(glmm_teststri$timeDAY)))
datestritrans <- lm(log(glmm_teststri$tri)~ log(glmm_teststri$timeDAY))
plot(datestritrans)
par(mfrow = c(1, 1))
plot(log(glmm_teststri$tri)~ log(glmm_teststri$timeDAY))
abline(lm(log(glmm_teststri$tri)~ log(glmm_teststri$timeDAY)))
sqrt(summary(datestritrans)$r.squared)
sqrt(summary(datestritrans)$r.squared)

par(mfrow = c(2, 2))
datestri<-lm((glmm_teststri$tri)~ (glmm_teststri$timeDAY))
summary(lm((glmm_teststri$tri)~ (glmm_teststri$timeDAY)))
plot(datestri)
par(mfrow = c(1, 1))

plot((glmm_teststri$tri)~ (glmm_teststri$timeDAY))
abline(lm((glmm_teststri$tri)~ (glmm_teststri$timeDAY)))

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

phys <- all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::mutate(
    SP= as.factor(SP)
  )|>
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
                                 ci_lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back_transform = FALSE,
                                 interval = "confidence")

#plots 
glu2 <- ggplot2::ggplot(data= phys, ggplot2::aes(x=SP, y=log(glu), color = SP))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = glumod, 
                                            terms = c('SP'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Glucose (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(text= ggplot2::element_text(size=20),
        axis.text.x=ggplot2::element_blank()
        )
glu2


#chol

phys <- all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::mutate(
    SP= as.factor(SP)
  )|>
  dplyr::filter(!is.na(chol))
cholmod<-lm(data=phys, log(chol)~ SP)

summary(cholmod)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(cholmod)
par(opar)

car::leveneTest(resid(cholmod), interaction(phys$Year, phys$SP))
boxplot(resid(cholmod) ~ interaction(phys$Year, phys$SP))



cholmod0 <- lm(data=phys, log(chol)~ 1)

options(na.action = "na.fail")
aic_lmCholSP<-MuMIn::model.sel(cholmod0, cholmod)

aic_lmCholSP

cholmod2019<-ggeffects::ggemmeans(model = cholmod, 
                                  terms = c('SP'),
                                  ci_lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back_transform = FALSE,
                                  interval = "confidence")

#plots 
chol2<-ggplot2::ggplot(data= phys, ggplot2::aes(x=SP, y=log(chol), color = SP))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = cholmod, 
                                            terms = c('SP'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Cholesterol (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(text= ggplot2::element_text(size=20),
        axis.text.x=ggplot2::element_blank())

chol2

#tri

phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::mutate(
    SP= as.factor(SP)
  )|>
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
                                 ci_lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back_transform = FALSE,
                                 interval = "confidence")

# 

tri2<-ggplot2::ggplot(data= phys, ggplot2::aes(x=SP, y=log(tri), color = SP))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = trimod, 
                                            terms = c('SP'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Triglycerides (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    text= ggplot2::element_text(size=20)
  )

tri2

#ket
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC)
  )|>
  dplyr::filter(Year == 2019)|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
  dplyr::mutate(
    SP= as.factor(SP)
  )|>
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
                                 ci_lvl = 0.95,
                                 type = "fe",
                                 typical = "mean",
                                 condition = NULL,
                                 back_transform = FALSE,
                                 interval = "confidence")

#plots 
ket2<-ggplot2::ggplot(data= phys, ggplot2::aes(x=SP, y=log(ket), color = SP))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = ketmod, 
                                            terms = c('SP'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Ketones (mmol/L)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    text= ggplot2::element_text(size=20)
  )
ket2

metabs2019<-cowplot::plot_grid(glu2, chol2, tri2, ket2, labels = c('A', 'B', 'C', 'D'), ncol = 2)
metabs2019
ggplot2::ggsave("plots/metabs2019_plotv2.png", metabs2019, dpi = 300, bg = "white", units = 'in', 
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
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
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
                                  ci_lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back_transform = FALSE,
                                  interval = "confidence")

#plots 
gluPEBO<-ggplot2::ggplot(data= phys, ggplot2::aes(x=Year, y=log(glu), color = Year))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  ggplot2::scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = glumodP, 
                                            terms = c('Year'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Glucose (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x=ggplot2::element_blank())
gluPEBO


#chol
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
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
                                   ci_lvl = 0.95,
                                   type = "fe",
                                   typical = "mean",
                                   condition = NULL,
                                   back_transform = FALSE,
                                   interval = "confidence")

#plots 

cholPEBO<-ggplot2::ggplot(data= phys, ggplot2::aes(x=Year, y=log(chol), color = Year))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  ggplot2::scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = cholmodP, 
                                            terms = c('Year'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Cholesterol (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x=ggplot2::element_blank())

cholPEBO


#tri
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
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
                                  ci_lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back_transform = FALSE,
                                  interval = "confidence")



#plots 

triPEBO<-ggplot2::ggplot(data= phys, ggplot2::aes(x=Year, y=log(tri), color = Year))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  ggplot2::scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = trimodP, 
                                            terms = c('Year'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Triglycerides (mg/dL)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()

triPEBO

#ket
phys<-all_deps|>
  dplyr::mutate(
    Year = lubridate::year(timeC),
    Year = as.factor(Year)
  )|>
  dplyr::filter(SP == "PEBO")|>
  dplyr::select(SP,Year, DepID, chol, glu, ket, tri)|>
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
                                  ci_lvl = 0.95,
                                  type = "fe",
                                  typical = "mean",
                                  condition = NULL,
                                  back_transform = FALSE,
                                  interval = "confidence")

#plots 

ketPEBO<-ggplot2::ggplot(data= phys, ggplot2::aes(x=Year, y=log(ket), color = Year))+ #, color = Spec
  ggplot2::geom_point(position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  ggplot2::scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggemmeans(model = ketmodP, 
                                            terms = c('Year'),
                                            ci_lvl = 0.95,
                                            type = "fe",
                                            typical = "mean",
                                            back_transform = FALSE,
                                            condition = NULL) ,
                  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Log-Ketones (mmol/L)")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()
ketPEBO


metabPEBO<-cowplot::plot_grid(gluPEBO, cholPEBO, triPEBO, ketPEBO, labels = c('A', 'B', 'C', 'D'), ncol = 2)
metabPEBO

ggplot2::ggsave("plots/metabsPEBO2018_19v2.png", metabPEBO , dpi = 300, bg = "white", units = 'in', 
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
#########################################################################################################################################
#PCA of metabolites: NOT USED
#########################################################################################################################################

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


