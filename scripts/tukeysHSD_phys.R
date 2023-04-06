library(tidyverse)
library(multcompView)
reg1i <- aov((ket) ~ as.factor(groups),  data = phystestsKET1)

tukey <- TukeyHSD(reg1i)

cld <- multcompLetters4(reg1i, tukey)

waje <- group_by(phystestsKET1, groups) %>% 
  summarise(
    w = mean(ket),
    sd = sd(ket)) %>% 
  arrange(desc(w))

cld <- as.data.frame.list(cld$`as.factor(groups)`)

waje$cld <- cld$Letters

print(waje)

D<-ggplot(waje, aes(groups, w))+
  geom_boxplot(data = phystestsKET1, aes( x = groups, y = ket, colour = groups))+
  labs(x = "EMD Clusters", y = "log(ket)")+
  geom_text(aes(label = cld, y = w + (1.15*sd)), vjust = -0.5)+
  theme(legend.position = "none")
