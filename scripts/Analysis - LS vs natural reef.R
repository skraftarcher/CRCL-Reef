#Compare the living shoreline reef with the natural reef

#load packages----
library(tidyverse)
library(vegan)
library(glmmTMB)
library(DHARMa)
library(easystats)

#Residualsx
glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  print(testZeroInflation(t1))
  plot(t1)
}

#load data----
wholedata.a<-bind_cols(read.csv("wdata/environment data.csv"), read.csv("wdata/community data abundance.csv"))|>
  filter(season!="Summer")
wholedata.b<-bind_cols(read.csv("wdata/environment data.csv"), read.csv("wdata/community data abundance.csv"))|>
  filter(season!="Summer")

#update reef types
wholedata.a<- wholedata.a%>%
  mutate(site=ifelse(location %in% c("LUMO3", "LUMO6"),
                     "Natural Reef",
                     "Living Shoreline"))
wholedata.a$season<-factor(wholedata.a$season,levels = c("Fall","Winter","Spring"))
wholedata.b<- wholedata.b%>%
  mutate(site=ifelse(location %in% c("LUMO3", "LUMO6"),
                     "Natural Reef",
                     "Living Shoreline"))
wholedata.b$season<-factor(wholedata.b$season,levels = c("Fall","Winter","Spring"))

#split data into env and community matrices----
mixenv<-wholedata.a|>
  select(date.retrieved,
         location,
         tray,
         season,
         yr,
         spr,
         diversity.abund,
         diversity.bmass,
         site)
mixcom.a<-wholedata.a|>
  select(-date.retrieved,
         -location,
         -tray,
         -season,
         -yr,
         -spr,
         -diversity.abund,
         -diversity.bmass,
         -site)
  
mixcom.a<-mixcom.a[,!colSums(mixcom.a)==0]

mixcom.b<-wholedata.b|>
  select(-date.retrieved,
         -location,
         -tray,
         -season,
         -yr,
         -spr,
         -diversity.abund,
         -diversity.bmass,
         -site)

mixcom.b<-mixcom.b[,!colSums(mixcom.b,na.rm=T)==0]


#explore taxa richness----
theme_set(theme_bw()+theme(panel.grid=element_blank()))
ggplot(data=mixenv)+
  geom_boxplot(aes(x=season, y=spr,fill=site))+
  ylab("Taxa Richness") +
  xlab("Season")


q2.taxr<-glmmTMB(spr~site*season,
                 data=wholedata.a,
                 family=genpois)


glmm.resids(q2.taxr)
summary(q2.taxr)

(tr.means<-estimate_means(q2.taxr,by="season"))
(contrasts <- estimate_contrasts(q2.taxr, contrast = "season"))

ggplot(mixenv, aes(x = season, y = spr)) +
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

(tr.means2<-estimate_means(q2.taxr,by="site"))
ggplot(mixenv, aes(x = site, y = spr)) +
  # Add base data
  geom_violin(aes(fill = site), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = tr.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = tr.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q2.taxr, contrast = "site",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")

# diversity based on abundance----
ggplot(data=mixenv)+
  geom_boxplot(aes(x=season, y=diversity.abund,fill=site))+
  ylab("Diversity - abundance") +
  xlab("Season")


q2.diva<-glmmTMB(diversity.abund~site*season,
                 data=wholedata.a)


glmm.resids(q2.diva)
summary(q2.diva)

(tr.means<-estimate_means(q2.diva,by="season"))
(contrasts <- estimate_contrasts(q2.diva, contrast = "season"))

ggplot(mixenv, aes(x = season, y = diversity.abund)) +
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

(tr.means2<-estimate_means(q2.diva,by="site"))
ggplot(mixenv, aes(x = site, y = diversity.abund)) +
  # Add base data
  geom_violin(aes(fill = site), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = tr.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = tr.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q2.diva, contrast = "site",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")


# diversity based on biomass----
ggplot(data=mixenv)+
  geom_boxplot(aes(x=season, y=diversity.bmass,fill=site))+
  ylab("Diversity - biomass") +
  xlab("Season")


q2.divab<-glmmTMB(diversity.bmass~site*season,
                 data=wholedata.a)


glmm.resids(q2.divab)
summary(q2.divab)

(tr.means<-estimate_means(q2.divab,by="season"))
(contrasts <- estimate_contrasts(q2.divab, contrast = "season"))

ggplot(mixenv, aes(x = season, y = diversity.bmass)) +
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

(tr.means2<-estimate_means(q2.divab,by="site"))
ggplot(mixenv, aes(x = site, y = diversity.bmass)) +
  # Add base data
  geom_violin(aes(fill = site), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = tr.means2, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = tr.means2,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) 


(contrasts <- estimate_contrasts(q2.divab, contrast = "site",by="season"))
contrasts$Contrast <- paste(contrasts$Level1, "-", contrasts$Level2)

# Visualise the changes in the differences
ggplot(contrasts, aes(x = season, y = Difference,group=Contrast)) +
  geom_ribbon(aes(fill = Contrast, ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(aes(colour = Contrast), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("Difference")

