library(MuMIn)
library(dplyr)

test1 <- readRDS(file.choose())

test1 <- test1 %>% 
  dplyr::select(glu ,tri , chol,  ket, PC1, sinuos, Spec, Year, latency, dep_id) %>% 
  filter(!is.na(glu)) %>% 
  filter(Spec == "PEBO")


str(test1)
# Without year, you don't need to worry about unequal variance
reg0 <- lm(formula = log(glu) ~ 1 , data = test1)
reg1 <- lm(formula = log(glu) ~ log(sinuos) , data = test1)
reg2 <- lm(formula = log(glu) ~ PC1 , data = test1) ###############DELETED method= 'ML' in this and all subsequent models
reg3 <- lm(formula = log(glu) ~ PC1 + log(sinuos) , data = test1)
reg4 <- lm(formula = log(glu) ~ PC1 * log(sinuos) ,  data = test1)
reg5 <- lm(formula = log(glu) ~ PC1 * log(latency) ,  data = test1)
reg6 <- lm(formula = log(glu) ~ log(latency),  data = test1)

#options(na.action = "na.fail")
aic_test1<-MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6)

aic_test1 # interaction terms in the table have a PLUS sign and not a coefficient

reg5 #shows the coeficient for the interaction as a number and not a plus sign
reg4 # same as above

### test with other data, does work
library(palmerpenguins)

data(package = 'palmerpenguins')
head(penguins)


library(MuMIn)


lm0 <- lm(data = penguins, formula = log(body_mass_g) ~ 1)
lm1 <- lm(data = penguins, formula = log(body_mass_g) ~ log(flipper_length_mm) )
lm2 <- lm(data = penguins, formula = log(body_mass_g) ~ log(bill_depth_mm)*log(flipper_length_mm))
lm3 <- lm(data = penguins, formula = log(body_mass_g) ~ log(flipper_length_mm)+ log(bill_depth_mm ))
lm4 <- lm(data = penguins, formula = log(body_mass_g) ~ log(flipper_length_mm)* log(bill_length_mm))

model.sel(lm0, lm1, lm2, lm3, lm4)