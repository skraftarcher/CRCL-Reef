#Aim 1 Stats Analysis

#load packages----
library(tidyverse)
library(vegan)
library(glmmTMB)
library(DHARMa)

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

#load data----
wholedata<-bind_cols(read.csv("wdata/environment data.csv"), read.csv("wdata/community data.csv"))%>%
  filter(location %in% c("CRCL.edge", "CRCL.channel"))%>%
  filter(season!="Winter")

#split data
crclenv<-wholedata[,1:7]
crclcom<-wholedata[,-1:-7]
crclcom<-crclcom[,!colSums(crclcom)==0]

#Taxa richness diff between marsh v channel/time

q1.taxr<-glmmTMB(spr~location*season,
                 data=wholedata,
                 family=poisson)

glmm.resids(q1.taxr)
summary(q1.taxr)

wholedata<-wholedata%>%
  mutate(pos=paste(location,season))
q1.taxr.kw<-kruskal.test(spr~pos,data=wholedata)
q1.taxr.kw


ce.per.fall<-adonis2(crclcom[crclenv$season=="Fall",]~location,data=crclenv[crclenv$season=="Fall",],permutations=9999)
ce.per.fall
ce.per.spring<-adonis2(crclcom[crclenv$season!="Fall",!colSums(crclcom)==0]~location,data=crclenv[crclenv$season!="Fall",],permutations=9999)
ce.per.spring
                       
#Spring Simper
spring.simper<-simper(comm=crclcom[crclenv$season!="Fall",],
                      group=crclenv$location[crclenv$season!="Fall"],
                      permutations=99999)
spring.simper.out<-as.data.frame(summary(spring.simper)[[1]])%>%
  filter(p<0.05)
spring.simper
summary(spring.simper)

#Spring Edge vs Channel
simp.res<-wholedata%>%
    filter(season=="Spring")%>%
  pivot_longer(8:72,names_to = "taxaID",values_to = "abund")%>%
  filter(taxaID %in% c("tun.2","bug.5","shmp.11","crb.6","fsh.2"))%>%
mutate(grph.name=case_when(
  taxaID=="tun.2"~"Unidentified Tunicate",
  taxaID=="bug.5"~"Culicidae spp",
  taxaID=="shmp.11"~"Daggerblade Grass Shrimp",
  taxaID=="crb.6"~"Blue Crab",
  taxaID=="fsh.2"~"Gobiosoma spp"),
  grph.name=factor(grph.name,levels=c("Unidentified Tunicate",
                                      "Culicidae spp",
                                      "Daggerblade Grass Shrimp",
                                      "Blue Crab",
                                      "Gobiosoma spp")))%>%
  group_by(location,grph.name)%>%
    summarize(m.abund=mean(abund,na.rm= T),
             sd.abund=sd(abund,na.rm=T))

ggplot(simp.res)+
  geom_errorbar(aes(x=location,
                    ymin=ifelse(m.abund-sd.abund<0,0.1,m.abund-sd.abund),
                    ymax=m.abund+sd.abund),
                width=0.1)+
  geom_bar(aes(y=m.abund,x=location,fill=location),stat = "identity")+
 facet_wrap(~grph.name,scales="free")+
  scale_fill_viridis_d(option="D",begin=0.4, end=0.8)+
  scale_x_discrete(labels=c("CRCL.edge"="Marsh Edge",
                            "CRCL.channel"="Channel"))+
  xlab("Location")+
  ylab("Mean Abundance")+
theme(legend.position="none")

