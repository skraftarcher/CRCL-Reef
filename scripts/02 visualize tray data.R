# visualize tray data

library(tidyverse)
library(readxl)
library(vegan)

theme_set(theme_bw()+theme(panel.grid = element_blank()))
# load data
tray.env<-read.csv("wdata/environment data.csv")%>%
  mutate(sampling=paste(season,yr))
tray.env$season<-factor(tray.env$season,levels=c("Fall","Winter","Spring"))
tray.env$sampling<-factor(tray.env$sampling,levels=c("Fall 2025","Winter 2026","Spring 2026"))
tray.com<-read.csv("wdata/community data.csv")

#multivariate visualization
tray.nmds<-metaMDS(tray.com)
plot(tray.nmds)

tray.scors<-data.frame(scores(tray.nmds,choices=c(1,2),display="sites"))
tray.env2<-bind_cols(tray.env,tray.scors)

ggplot(data=tray.env2)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=location,shape=season),size=6,alpha=.8)+
  scale_color_manual(values=c("#663390","#FF6600"))+
  facet_wrap(~sampling)

ggsave("figures/crcl_prelimdata multivariate.jpg")

# species richness 
ggplot(data=tray.env)+
  geom_boxplot(aes(x=sampling,y=spr,fill=location))+
  scale_fill_manual(values=c("#663390","#FF6600"),name="")+
  ylab("Taxa richness")+
  xlab("Sampling")

ggsave("figures/crcl_prelimdata taxa richness.jpg")

# species diversity 
ggplot(data=tray.env)+
  geom_boxplot(aes(x=sampling,y=diversity,fill=location))+
  scale_fill_manual(values=c("#663390","#FF6600"),name="")+
  ylab("Taxa diversity")+
  xlab("Sampling")

ggsave("figures/crcl_prelimdata taxa diversity.jpg")
