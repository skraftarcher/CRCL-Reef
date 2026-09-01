#Aim 2 Stats Analyses

#load packages----
library(tidyverse)
library(vegan)
library(glmmTMB)
library(DHARMa)

#Residualsx
glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

#load data----
wholedata<-bind_cols(read.csv("wdata/environment data.csv"), read.csv("wdata/community data.csv"))%>%
  filter(season!="Winter")

#make CRCL one
wholedata<- wholedata%>%
  mutate(site=ifelse(location %in% c("LUMO3", "LUMO6"),
                     "Natural Reef",
                     "Living Shoreline"))

#split data
mixenv<-wholedata[,c(1:7,75)]
mixcom<-wholedata[,c(-1:-7,-75)]
mixcom<-mixcom[,!colSums(mixcom)==0]




#graph
ggplot(data=mixenv)+
  geom_boxplot(aes(x=season, y=spr,fill=site))+
  ylab("Taxa Richness") +
  xlab("Season")


q2.taxr<-glmmTMB(spr~site*season,
                 data=wholedata,
                 family=compois)


glmm.resids(q2.taxr)
summary(q2.taxr)

wholedata<-wholedata%>%
  mutate(pos=paste(location,season))
q2.taxr.kw<-kruskal.test(spr~pos,data=wholedata)
q2.taxr.kw


ce.per.fall<-adonis2(mixcom[mixenv$season=="Fall",]~site,data=mixenv[mixenv$season=="Fall",],permutations=9999)
ce.per.fall
ce.per.spring<-adonis2(mixcom[mixenv$season!="Fall",!colSums(mixcom)==0]~site,data=mixenv[mixenv$season!="Fall",],permutations=9999)
ce.per.spring
meandist(dist = vegdist(mixcom[mixenv$season=="Fall",]),
         grouping = mixenv$site[mixenv$season=="Fall"])
#fall simper
fall.simper<-simper(comm=mixcom[mixenv$season=="Fall",],
                      group=mixenv$site[mixenv$season=="Fall"],
                      permutations=9999)
(fall.simper.out<-as.data.frame(summary(fall.simper)[[1]])%>%
  filter(p<=0.05)%>%
  filter(average>0)%>%
    mutate(avdist=0.7173928,
           cont.tocumulative.sum=average/avdist,
           taxaID=row.names(.))%>%
  arrange(-cont.tocumulative.sum))

fall.simper.out$cumulative.sum<-cumsum(fall.simper.out$cont.tocumulative)
fall.simper
summary(fall.simper)
fall.simper.out$taxaID<-factor(fall.simper.out$taxaID,levels=fall.simper.out$taxaID)
ggplot(data=fall.simper.out)+
  geom_bar(aes(x=taxaID,y=cont.tocumulative.sum),stat="identity")


#spring simper

meandist(dist = vegdist(mixcom[mixenv$season!="Fall",]),
         grouping = mixenv$site[mixenv$season!="Fall"])
spring.simper<-simper(comm=mixcom[mixenv$season!="Fall",],
                      group=mixenv$site[mixenv$season!="Fall"],
                      permutations=9999)
(spring.simper.out<-as.data.frame(summary(spring.simper)[[1]])%>%
  filter(p<0.05)%>%
  filter(average>0)%>%
  mutate(avdist=0.8368986,
         cont.tocumulative.sum=average/avdist,
         taxaID=row.names(.))%>%
  arrange(-cont.tocumulative.sum))
spring.simper.out$cumulative.sum<-cumsum(spring.simper.out$cont.tocumulative)
spring.simper
summary(spring.simper)
spring.simper.out$taxaID<-factor(spring.simper.out$taxaID,levels=spring.simper.out$taxaID)
ggplot(data=spring.simper.out)+
  geom_bar(aes(x=taxaID,y=cont.tocumulative.sum),stat="identity")

#taxa both
fall.taxa<-row.names(fall.simper.out)
spring.taxa<-row.names(spring.simper.out)
(both.taxa<-data.frame(taxaID=fall.taxa[fall.taxa %in% spring.taxa],
                       in.both=1))

fall.simper.out<-left_join(fall.simper.out,both.taxa)%>%
  mutate(in.both=ifelse(is.na(in.both),"Fall only","Both seasons"))
fall.simper.out$taxaID<-factor(fall.simper.out$taxaID,levels=fall.simper.out$taxaID)

ggplot(data=fall.simper.out)+
  geom_bar(aes(x=taxaID,y=cont.tocumulative.sum,fill=in.both),stat="identity")


spring.simper.out<-left_join(spring.simper.out,both.taxa)%>%
  mutate(in.both=ifelse(is.na(in.both),"Spring only","Both seasons"))
spring.simper.out$taxaID<-factor(spring.simper.out$taxaID,levels=spring.simper.out$taxaID)

ggplot(data=spring.simper.out)+
  geom_bar(aes(x=taxaID,y=cont.tocumulative.sum,fill=in.both),stat="identity")

#Did channel approach natural faster
mixNMDS<-metaMDS(mixcom)
plot(mixNMDS)
wout<-data.frame(scores(mixNMDS, choices = c(1,2),display="sites"))

mixenv2<-bind_cols(mixenv,wout)%>%
  mutate(pos=paste(site,season))

ggplot(data=mixenv2) +
  geom_point(aes(x=NMDS1, y=NMDS2, color= site),size=4)+
  stat_ellipse(aes(x=NMDS1, y=NMDS2, color=site))+
  facet_wrap(~season)

#calc cent
nat.reefs<-mixenv2%>%
  filter(site=="Natural Reef")%>%
  group_by(season)%>%
  summarize(nr.NMDS1=mean(NMDS1),
            nr.NMDS2=mean(NMDS2))
env.ls<-mixenv2%>%
  filter(site=="Living Shoreline")%>%
  left_join(nat.reefs)%>%
  mutate(dist.to.nr=sqrt(((NMDS1-nr.NMDS1)^2)+(NMDS2-nr.NMDS2)^2))

ggplot(mixenv2)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=site))+
  geom_point(aes(x=nr.NMDS1, y=nr.NMDS2),
             color="black", size=4, data=nat.reefs)+
  stat_ellipse(aes(x=NMDS1,y=NMDS2,color=site))+
  facet_wrap(~season)

dis.to.nr.aov<-aov(dist.to.nr~location*season,data=env.ls)
par(mfrow=c(2,2))
plot(dis.to.nr.aov)

summary(dis.to.nr.aov)

ggplot(data=env.ls)+
  geom_boxplot(aes(x=location,y=dist.to.nr,fill=season))


#Did channel approach natural faster without tun.2
mixNMDS2<-metaMDS(mixcom[,colnames(mixcom)!="tun.2"])
plot(mixNMDS2)
wout<-data.frame(scores(mixNMDS2, choices = c(1,2),display="sites"))

mixenv3<-bind_cols(mixenv,wout)%>%
  mutate(pos=paste(site,season))

ggplot(data=mixenv3) +
  geom_point(aes(x=NMDS1, y=NMDS2, color= site),size=4)+
  stat_ellipse(aes(x=NMDS1, y=NMDS2, color=site))+
  facet_wrap(~season)

#calc cent
nat.reefs2<-mixenv3%>%
  filter(site=="Natural Reef")%>%
  group_by(season)%>%
  summarize(nr.NMDS1=mean(NMDS1),
            nr.NMDS2=mean(NMDS2))
env.ls2<-mixenv3%>%
  filter(site=="Living Shoreline")%>%
  left_join(nat.reefs2)%>%
  mutate(dist.to.nr=sqrt(((NMDS1-nr.NMDS1)^2)+(NMDS2-nr.NMDS2)^2))

ggplot(mixenv3)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=site))+
  geom_point(aes(x=nr.NMDS1, y=nr.NMDS2),
             color="black", size=4, data=nat.reefs)+
  stat_ellipse(aes(x=NMDS1,y=NMDS2,color=site))+
  facet_wrap(~season)

dis.to.nr.aov2<-aov(dist.to.nr~location*season,
                   data=env.ls2)
par(mfrow=c(2,2))
plot(dis.to.nr.aov2)

summary(dis.to.nr.aov2)

ggplot(data=env.ls)+
  geom_boxplot(aes(x=location,y=dist.to.nr,fill=season))
