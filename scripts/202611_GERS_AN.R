library(tidyverse)
library(readxl)
library(vegan)
library(RColorBrewer)

theme_set(theme_bw() + 
            theme(panel.grid = element_blank(),
                  axis.text = element_text(size = 18),
                  axis.title = element_text(size= 22),
                  strip.text = element_text(size = 20, face = "bold"),
                  plot.background = element_rect(fill = "#F0F0E0", color = NA),
                  panel.background = element_rect(fill = "#F0F0E0", color = NA),
                  strip.background = element_rect(fill = "#F0F0E0", color = NA),
                  legend.background = element_rect(fill = "#F0F0E0", color = NA),
                  text = element_text(family = "serif")))



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

methods.joint<- rbind(tray.sum1, trap.sum1)
methods.joint$season<- factor(methods.joint$season,levels=c("fall25","winter25","spring26","summer26", "fall26"))

season.labels<- c(`fall25` = "Fall 2025", `winter25` = "Winter 2025", 
                  `spring26` = "Spring 2026", `summer26` = "Summer 2026", 
                  `fall26` = "Fall 2026")
method.labels<- c(`crab` = "Crab trap", `minnow` = "Minnow\ntrap", `tray` = "Habitat\ntray")
cols <- colorRampPalette(brewer.pal(12, "Set3"))
set.seed(44)
spp_pal <- sample(cols(length(unique(methods.joint$taxaID))))

spp_pal2<- data.frame(spp = sort(unique(methods.joint$taxaID)), col = spp_pal)


methods.plot<- ggplot(methods.joint, aes(x = location, y = abund)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  facet_grid(type ~ season, labeller = labeller(season = season.labels, type = function(x) {rep("", length(x))})) +
  labs(y = "Abundance", x = " ")

ggsave(filename = "figures/methods_plot.png", methods.plot, width = 12, height = 6.5, units = "in")

methods.plot2<- ggplot(methods.joint, aes(x = location, y = abund, fill = taxaID)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.25) +
  facet_grid(type ~ season, labeller = labeller(season = season.labels, type = function(x) {rep("", length(x))})) +
  labs(y = "Abundance", x = " ") +
  scale_fill_manual(values = spp_pal) +
  theme(legend.position = "none")
methods.plot2

ggsave(filename = "figures/methods_plot2.png", methods.plot2, width = 12, height = 6.5, units = "in")

### minnow traps only - organism abundance over season
mtrap.sum<- subset(methods.joint, subset = type == "minnow")
mtrap.pal<- subset(spp_pal2, subset = spp %in% unique(mtrap.sum$taxaID))

ggplot(mtrap.sum, aes(x = location, y = abund, fill = taxaID)) +
  geom_bar(position = "stack", stat = "identity", color = "black", linewidth = 0.25) +
  facet_grid(~ season) +
  scale_fill_manual(values = mtrap.pal$col)
#shrimp dominant throughout, more blue crabs and killifish emerging over time, major ctenophore presence in fall
# fewer white shrimp post= reef installation

### dominant species - how do sizes differ between channel/edge?

ce.pal<- c("#c8e1E0", "#c8eac0")

shmp.lengths<- subset(trap.lengths, subset = taxaID == "shmp-1")
ggplot(shmp.lengths, aes(x = )) +
  geom_violin() +
  facet_grid(~ season) +
  scale_fill_manual(values = ce.pal)



### does abundance differ between reef sides and by season?
min.trap.sum<- subset()


trap.mod1<- lm()





