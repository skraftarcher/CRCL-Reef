# script to visualize trap data
#load packages----
library(tidyverse)
library(readxl)

#set ggplot theme----
theme_set(theme_bw()+theme(panel.grid = element_blank()))

#load data----
trap.com.cpue<-read.csv("wdata/trap community data cpue.csv")
trap.env<-read.csv("wdata/trap env data.csv")
trap.env$season<-factor(trap.env$season,levels=c("fall25","winter25","spring26","summer26"))
trap.lengths<-read.csv("wdata/trap lengths.csv")
trap.lengths$season<-factor(trap.lengths$season,levels=c("fall25","winter25","spring26","summer26"))
trap.pe<-read.csv("wdata/trap shrimp parasites and eggs.csv")

# multivariate trap data
trap.nmds<-metaMDS(trap.com.cpue[rowSums(trap.com.cpue)!=0,])
plot(trap.nmds)

trap.scors<-data.frame(scores(trap.nmds,choices=c(1,2),display="sites"))
trap.env2<-bind_cols(trap.env[rowSums(trap.com.cpue)!=0,],trap.scors)

ggplot(data=trap.env2)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=loc),size=6,alpha=.8)+
  scale_color_manual(values=c("#663390","#FF6600"))+
  facet_wrap(~season)

ggsave("figures/crcl_trap_multivariate.jpg")

# trap taxa richness
ggplot(trap.env)+
  geom_boxplot(aes(x=season,y=spr,fill=loc))+
  scale_fill_manual(values=c("#663390","#FF6600"))

# trap diversity from CPUE
ggplot(trap.env)+
  geom_boxplot(aes(x=season,y=div.cpue,fill=loc))+
  scale_fill_manual(values=c("#663390","#FF6600"))

# trap diversity from abundance
ggplot(trap.env)+
  geom_boxplot(aes(x=season,y=div.abund,fill=loc))+
  scale_fill_manual(values=c("#663390","#FF6600"))

# look at shrimp size
ggplot(data=trap.lengths)+
  geom_density(aes(x=length,fill=location),alpha=.5)+
  facet_wrap(~season)

ggplot(data=trap.lengths%>%filter(eggs==1))+
  geom_density(aes(x=length,fill=location),alpha=.5)+
  facet_wrap(~season)

ggplot(data=trap.lengths%>%filter(parasites==1))+
  geom_density(aes(x=length,fill=location),alpha=.5)+
  facet_wrap(~season)
  