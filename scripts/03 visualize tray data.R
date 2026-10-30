# visualize tray data

library(tidyverse)
library(readxl)
library(vegan)

theme_set(theme_bw()+theme(panel.grid = element_blank()))
# load data
tray.env<-read.csv("wdata/environment data.csv")%>%
  mutate(sampling=paste(season,yr))
tray.env$season<-factor(tray.env$season,levels=c("Fall","Winter","Spring","Summer"))
tray.env$sampling<-factor(tray.env$sampling,levels=c("Fall 2025","Winter 2026","Spring 2026","Summer 2026"))
tray.abund<-read.csv("wdata/community data abundance.csv")
tray.mass<-read.csv("wdata/community data biomass.csv")

#multivariate visualization - abundance
trayabund.nmds<-metaMDS(tray.abund)
plot(trayabund.nmds)

tray.scors<-data.frame(scores(trayabund.nmds,choices=c(1,2),display="sites"))
tray.env2<-bind_cols(tray.env,tray.scors)

ggplot(data=tray.env2)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=location,shape=season),size=6,alpha=.8)+
  # scale_color_manual(values=c("#663390","#FF6600"))+
  facet_wrap(~sampling)

ggsave("figures/crcl_prelimdata multivariate.jpg")

#multivariate visualization - biomass

traymass.nmds<-metaMDS(tray.mass)
plot(traymass.nmds)

tray.scors2<-data.frame(scores(traymass.nmds,choices=c(1,2),display="sites"))
tray.env3<-bind_cols(tray.env,tray.scors2)

ggplot(data=tray.env3)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=location,shape=season),size=6,alpha=.8)+
  # scale_color_manual(values=c("#663390","#FF6600"))+
  facet_wrap(~sampling)

ggsave("figures/crcl_prelimdata multivariate.jpg")

# species richness 
ggplot(data=tray.env)+
  geom_boxplot(aes(x=sampling,y=spr,fill=location))+
  # scale_fill_manual(values=c("#663390","#FF6600"),name="")+
  ylab("Taxa richness")+
  xlab("Sampling")

ggsave("figures/crcl_prelimdata taxa richness.jpg")

# species diversity - abundance 
ggplot(data=tray.env)+
  geom_boxplot(aes(x=sampling,y=diversity.abund,fill=location))+
  # scale_fill_manual(values=c("#663390","#FF6600"),name="")+
  ylab("Taxa diversity")+
  xlab("Sampling")

ggsave("figures/crcl_prelimdata taxa abundance diversity.jpg")

# species diversity - biomass 
ggplot(data=tray.env)+
  geom_boxplot(aes(x=sampling,y=diversity.bmass,fill=location))+
  # scale_fill_manual(values=c("#663390","#FF6600"),name="")+
  ylab("Taxa diversity")+
  xlab("Sampling")

ggsave("figures/crcl_prelimdata taxa biomass diversity.jpg")