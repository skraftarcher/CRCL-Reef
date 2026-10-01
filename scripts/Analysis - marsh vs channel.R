#Aim 1 Stats Analysis

#load packages----
library(tidyverse)
library(vegan)
library(glmmTMB)
library(DHARMa)
library(easystats)

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  print(testZeroInflation(t1))
  plot(t1)
}

#load data----
tray.env<-read.csv("wdata/environment data.csv")%>%
  mutate(sampling=paste(season,yr))
tray.env$season<-factor(tray.env$season,levels=c("Fall","Winter","Spring","Summer"))
tray.env$sampling<-factor(tray.env$sampling,levels=c("Fall 2025","Winter 2026","Spring 2026","Summer 2026"))
tray.com.abd<-read.csv("wdata/community data abundance.csv")
tray.com.bms<-read.csv("wdata/community data biomass.csv")
wholedata<-bind_cols(tray.env,tray.com.abd)|>
  filter(location %in% c("CRCL.edge", "CRCL.channel"))
wholedata.bms<-bind_cols(tray.env,tray.com.bms)|>
  filter(location %in% c("CRCL.edge", "CRCL.channel"))

crclcom.bms<-wholedata.bms[,-1:-9]
crclcom.bms<-crclcom.bms[,!colSums(crclcom.bms)==0]
# set theme for visualization plots
theme_set(theme_bw()+theme(panel.grid = element_blank()))
#split data
crclenv<-wholedata[,1:9]
crclcom.abd<-wholedata[,-1:-9]
crclcom.abd<-crclcom.abd[,!colSums(crclcom.abd)==0]
crclcom.abd<-crclcom.abd[,-grep(pattern="wtf",x=colnames(crclcom.abd))]
#Taxa richness diff between marsh v channel/time

q1.taxr<-glmmTMB(spr~location*season,
                 data=wholedata,
                 family=genpois)

glmm.resids(q1.taxr)
summary(q1.taxr)

(tr.means<-estimate_means(q1.taxr,by="season"))
(contrasts <- estimate_contrasts(q1.taxr, contrast = "season"))

ggplot(crclenv, aes(x = season, y = spr)) +
  # Add base data
  geom_violin(aes(fill = season), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = tr.means, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = tr.means,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 

(tr.means2<-estimate_means(q1.taxr,by="location"))
ggplot(crclenv, aes(x = location, y = spr)) +
  # Add base data
  geom_violin(aes(fill = location), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = tr.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = tr.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q1.taxr, contrast = "location",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")

# now look at differences in diversity based on abundance
q1.diva<-glmmTMB(diversity.abund~location*season,
                 data=wholedata)

glmm.resids(q1.diva)
summary(q1.diva)

(diva.means<-estimate_means(q1.diva,by="season"))
(contrasts <- estimate_contrasts(q1.diva, contrast = "season"))

ggplot(crclenv, aes(x = season, y = diversity.abund)) +
  # Add base data
  geom_violin(aes(fill = season), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = diva.means, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = diva.means,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 

(diva.means2<-estimate_means(q1.diva,by="location"))
ggplot(crclenv, aes(x = location, y = diversity.abund)) +
  # Add base data
  geom_violin(aes(fill = location), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = diva.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = diva.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q1.diva, contrast = "location",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")

# now look at differences in diversity based on biomass
q1.divb<-glmmTMB(diversity.bmass~location*season,
                 data=wholedata)

glmm.resids(q1.divb)
summary(q1.divb)

(divb.means<-estimate_means(q1.divb,by="season"))
(contrasts <- estimate_contrasts(q1.divb, contrast = "season"))

ggplot(crclenv, aes(x = season, y = diversity.bmass)) +
  # Add base data
  geom_violin(aes(fill = season), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = divb.means, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = divb.means,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 

(divb.means2<-estimate_means(q1.divb,by="location"))
ggplot(crclenv, aes(x = location, y = diversity.bmass)) +
  # Add base data
  geom_violin(aes(fill = location), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = divb.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = divb.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q1.divb, contrast = "location",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")

# now look at multivariate based on abundance

ce.per<-adonis2(crclcom.abd~location*season,data=crclenv,permutations=9999)
ce.per
# interaction between location and season is significant
# visualize community structure
crclmds<-metaMDS(crclcom.abd)
crclscores<-scores(crclmds,choices = c(1,2),display="sites")
crclenv2<-bind_cols(crclenv,crclscores)
ggplot(data=crclenv2,aes(x=NMDS1,y=NMDS2,color=season,shape=location))+
  geom_point()+
  stat_ellipse()+
  facet_wrap(~season)

#Simper
crclenv$grps<-paste(crclenv$location,crclenv$sampling)
crcl.simper<-simper(comm=crclcom.abd,
                      group=crclenv$grps,
                      permutations=99999)
sfall<-data.frame(crcl.simper$`CRCL.edge Fall 2025_CRCL.channel Fall 2025`)|>
  mutate(season="fall")|>
  filter(p<=0.05)
swinter<-data.frame(crcl.simper$`CRCL.edge Winter 2026_CRCL.channel Winter 2026`)|>
  mutate(season="winter")|>
  filter(p<=0.05)
sspring<-data.frame(crcl.simper$`CRCL.edge Spring 2026_CRCL.channel Spring 2026`)|>
  mutate(season="spring")|>
  filter(p<=0.05)
ssummer<-data.frame(crcl.simper$`CRCL.edge Summer 2026_CRCL.channel Summer 2026`)|>
  mutate(season="summer")|>
  filter(p<=0.05)



#visualize average abundance of each significant taxa
longdat<-wholedata|>
  pivot_longer(-date.retrieved:-sampling,names_to="species",values_to="abund")


winter.simp<-longdat|>
  filter(sampling=="Winter 2026")|>
  filter(species %in% swinter$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mabund=mean(abund),
            sdabund=sd(abund))

wintersd<-winter.simp[,c(1,2,3,5)]

winter.simp<-winter.simp|>
  select(-sdabund)|>
  pivot_wider(names_from = location,values_from=mabund)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mabund")|>
  left_join(wintersd)

ggplot(data=winter.simp)+
  geom_errorbar(aes(x=species,ymin=mabund,ymax=mabund+sdabund,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mabund,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")
  

spring.simp<-longdat|>
  filter(sampling=="Spring 2026")|>
  filter(species %in% sspring$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mabund=mean(abund),
            sdabund=sd(abund))

springsd<-spring.simp[,c(1,2,3,5)]

spring.simp<-spring.simp|>
  select(-sdabund)|>
  pivot_wider(names_from = location,values_from=mabund)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mabund")|>
  left_join(springsd)

ggplot(data=spring.simp)+
  geom_errorbar(aes(x=species,ymin=mabund,ymax=mabund+sdabund,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mabund,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")

summer.simp<-longdat|>
  filter(sampling=="Summer 2026")|>
  filter(species %in% ssummer$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mabund=mean(abund),
            sdabund=sd(abund))

summersd<-summer.simp[,c(1,2,3,5)]

summer.simp<-summer.simp|>
  select(-sdabund)|>
  pivot_wider(names_from = location,values_from=mabund)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mabund")|>
  left_join(summersd)

ggplot(data=summer.simp)+
  geom_errorbar(aes(x=species,ymin=mabund,ymax=mabund+sdabund,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mabund,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")

# redo all multivariate stuff based on biomass----
ce.per.bms<-adonis2(crclcom.bms~location*season,data=crclenv,permutations=9999)
ce.per
# interaction between location and season is significant
# visualize community structure
crclmds.bms<-metaMDS(crclcom.bms)
crclscores.bms<-scores(crclmds.bms,choices = c(1,2),display="sites")
crclenv2<-bind_cols(crclenv,crclscores.bms)
ggplot(data=crclenv2,aes(x=NMDS1,y=NMDS2,color=season,shape=location))+
  geom_point()+
  stat_ellipse()+
  facet_wrap(~season)

#Simper
crclenv$grps<-paste(crclenv$location,crclenv$sampling)
crcl.simper.bms<-simper(comm=crclcom.bms,
                    group=crclenv$grps,
                    permutations=99999)
sfall<-data.frame(crcl.simper.bms$`CRCL.edge Fall 2025_CRCL.channel Fall 2025`)|>
  mutate(season="fall")|>
  filter(p<=0.05)
swinter<-data.frame(crcl.simper.bms$`CRCL.edge Winter 2026_CRCL.channel Winter 2026`)|>
  mutate(season="winter")|>
  filter(p<=0.05)
sspring<-data.frame(crcl.simper.bms$`CRCL.edge Spring 2026_CRCL.channel Spring 2026`)|>
  mutate(season="spring")|>
  filter(p<=0.05)
ssummer<-data.frame(crcl.simper.bms$`CRCL.edge Summer 2026_CRCL.channel Summer 2026`)|>
  mutate(season="summer")|>
  filter(p<=0.05)



#visualize average abundance of each significant taxa
longdat.bms<-wholedata.bms|>
  pivot_longer(-date.retrieved:-sampling,names_to="species",values_to="bmass")

fall.simp.bms<-longdat.bms|>
  filter(sampling=="Fall 2025")|>
  filter(species %in% sfall$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mbmass=mean(bmass),
            sdbmass=sd(bmass))

fallsd.bms<-fall.simp.bms[,c(1,2,3,5)]

fall.simp.bms<-fall.simp.bms|>
  select(-sdbmass)|>
  pivot_wider(names_from = location,values_from=mbmass)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mbmass")|>
  left_join(fallsd.bms)

ggplot(data=fall.simp.bms)+
  geom_errorbar(aes(x=species,ymin=mbmass,ymax=mbmass+sdbmass,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mbmass,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")

winter.simp.bms<-longdat.bms|>
  filter(sampling=="Winter 2026")|>
  filter(species %in% swinter$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mbmass=mean(bmass),
            sdbmass=sd(bmass))

wintersd.bms<-winter.simp.bms[,c(1,2,3,5)]

winter.simp.bms<-winter.simp.bms|>
  select(-sdbmass)|>
  pivot_wider(names_from = location,values_from=mbmass)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mbmass")|>
  left_join(wintersd.bms)

ggplot(data=winter.simp.bms)+
  geom_errorbar(aes(x=species,ymin=mbmass,ymax=mbmass+sdbmass,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mbmass,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")


spring.simp.bms<-longdat.bms|>
  filter(sampling=="Spring 2026")|>
  filter(species %in% sspring$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mbmass=mean(bmass),
            sdbmass=sd(bmass))

springsd.bms<-spring.simp.bms[,c(1,2,3,5)]

spring.simp.bms<-spring.simp.bms|>
  select(-sdbmass)|>
  pivot_wider(names_from = location,values_from=mbmass)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mbmass")|>
  left_join(springsd.bms)

ggplot(data=spring.simp.bms)+
  geom_errorbar(aes(x=species,ymin=mbmass,ymax=mbmass+sdbmass,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mbmass,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")

summer.simp.bms<-longdat.bms|>
  filter(sampling=="Summer 2026")|>
  filter(species %in% ssummer$species)|>
  # group_by(sampling,species)|>
  summarize(.by=c(sampling,location,species),
            mbmass=mean(bmass),
            sdbmass=sd(bmass))

summersd.bms<-summer.simp.bms[,c(1,2,3,5)]

summer.simp.bms<-summer.simp.bms|>
  select(-sdbmass)|>
  pivot_wider(names_from = location,values_from=mbmass)|>
  mutate(where.higher=ifelse(CRCL.edge>CRCL.channel,"Edge","Channel"))|>
  pivot_longer(CRCL.edge:CRCL.channel,names_to = "location",values_to="mbmass")|>
  left_join(summersd.bms)

ggplot(data=summer.simp.bms)+
  geom_errorbar(aes(x=species,ymin=mbmass,ymax=mbmass+sdbmass,group=location),position = position_dodge(0.9))+
  geom_bar(aes(x=species,y=mbmass,fill=location),stat="identity",position = position_dodge())+
  facet_wrap(~where.higher,scales="free")
