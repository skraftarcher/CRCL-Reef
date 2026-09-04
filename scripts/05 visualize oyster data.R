# script to visualize oyster data
#load packages----
library(tidyverse)
library(readxl)
library(glmmTMB)
library(DHARMa)

#set ggplot theme----
theme_set(theme_bw()+theme(panel.grid = element_blank()))

#load data
oys.count<-crcl.trps.dr<-read_xlsx("odata/CRCL Oysters.xlsx",sheet = 2)
oys.size<-crcl.trps.dr<-read_xlsx("odata/CRCL Oysters.xlsx",sheet = 3)

#visualize oyster size data
ggplot(data=oys.size)+
  geom_density(aes(x=height,fill=position),alpha=.5)

ggplot(data=oys.size)+
  geom_boxplot(aes(y=height,fill=position),alpha=.5)

#visualize oyster count data
ggplot(data=oys.count)+
  geom_density(aes(x=live.oysters,fill=position),alpha=.5)

ggplot(data=oys.count)+
  geom_boxplot(aes(y=live.oysters,fill=position),alpha=.5)

# quick exploratory analysis
glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

size.lmer<-glmmTMB(height~position+(1|transect),
                   data=oys.size,
                   family=nbinom2)

glmm.resids(size.lmer)

summary(size.lmer)

# outside oysters are larger
count.lmer<-glmmTMB(live.oysters~position+(1|transect),
                   data=oys.count,
                   family=nbinom2)

glmm.resids(count.lmer)

summary(count.lmer)
#there are more oysters in the middle

