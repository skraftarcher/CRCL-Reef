library(tidyverse)
library(readxl)
library(vegan)

#load tray data----
tray.env<-read.csv("wdata/environment data.csv")%>%
  mutate(sampling=paste(season,yr))
tray.env$season<-factor(tray.env$season,levels=c("Fall","Winter","Spring","Summer"))
tray.env$sampling<-factor(tray.env$sampling,levels=c("Fall 2025","Winter 2026","Spring 2026","Summer 2026"))
tray.abund<-read.csv("wdata/community data abundance.csv")
tray.mass<-read.csv("wdata/community data biomass.csv")

#load trap data----
trap.com.cpue<-read.csv("wdata/trap community data cpue.csv")
trap.env<-read.csv("wdata/trap env data.csv")
trap.env$season<-factor(trap.env$season,levels=c("fall25","winter25","spring26","summer26", "fall26"))
trap.lengths<-read.csv("wdata/trap lengths.csv")
trap.lengths$season<-factor(trap.lengths$season,levels=c("fall25","winter25","spring26","summer26", "fall26"))
trap.pe<-read.csv("wdata/trap shrimp parasites and eggs.csv")


#how do our sampling methods differ in total catch
trp$season<- factor(trp$season,levels=c("fall25","winter25","spring26","summer26", "fall26"))
#trp was made in file "02 organize data" 

trap.sum1<- trp %>%
  mutate(type = case_when(grepl("-C", TrapID) ~ "crab",
                          grepl("-M", TrapID) ~ "minnow")) %>%
  group_by(season, location, type, taxaID = taxa.verified) %>%
  summarize(abund = n()) %>%
  mutate(location = case_when(location == "e" ~ "edge",
                              location == "o" ~ "channel"))

tray.sum1<- crcl.trays2 %>%
  mutate(type = "tray") %>%
  group_by(season, location, type, taxaID) %>%
  summarize(abund = sum(abund)) %>%
  mutate(location = case_when(location == "CRCL.edge" ~ "edge",
                              location == "CRCL.channel" ~ "channel"))



### does abundance differ between reef sides and by season?
trap.div.sum<- trp %>%
  filter(!grepl('-C', TrapID)) %>%
  group_by()


trap.mod1<- lm()





theme_set(theme_bw() + 
            theme(panel.grid = element_blank(),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size= 18),
                  plot.background = element_rect(fill = "#F0F0E0", color = NA),
                  panel.background = element_rect(fill = "#F0F0E0", color = NA),
                  strip.background = element_rect(fill = "#F0F0E0", color = NA),
                  legend.background = element_rect(fill = "#F0F0E0", color = NA),
                  text = element_text(family = "serif")))