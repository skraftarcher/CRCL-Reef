library(multcompView)
library(ragg)

### TRAP DATA

# load trap data ---
trap.com.cpue<-read.csv("wdata/trap community data cpue.csv")
trap.env<-read.csv("wdata/trap env data.csv")
trap.env$season<-factor(trap.env$season,levels=c("fall25","winter25","spring26","summer26"))
trap.lengths<-read.csv("wdata/trap lengths.csv")
trap.lengths$season<-factor(trap.lengths$season,levels=c("fall25","winter25","spring26","summer26"))
trap.pe<-read.csv("wdata/trap shrimp parasites and eggs.csv")


# how does shrimp length change between reef sides and across seasons?
shmp.lengths<- trap.lengths[trap.lengths$taxaID == "shmp-1",]

ggplot(shmp.lengths, aes(x = season, y = length, color = location)) +
  geom_boxplot()
#doesn't look like much

shmp.aov<- aov(length~season*location, shmp.lengths)
anova(shmp.aov)
# interaction of season and location
# strong main effect of season, marginal effect of location
# may have a statistical vs ecological significance thing here
shmp.tukey<- TukeyHSD(shmp.aov)
shmp.tukey.narm <-shmp.tukey$`season:location`[c(2:7, 14:28),]
shmp.letters<- multcompLetters(shmp.tukey.narm[,4])$Letters

shmp.letters2<- c(unname(shmp.letters)[c(7, 3)], " ", unname(shmp.letters)[c(4, 1, 5, 2, 6)])

# does overall size of organism differ between sides?
length.aov<- aov(length~season*location, trap.lengths)
anova(length.aov)
#strong interaction

ggplot(trap.lengths, aes(x = season, y = length, color = location)) +
  geom_violin()
# likely strong effect of kilifish

# how does prevalence of kilifish change?
ggplot(trap.lengths, aes(x = location, fill = taxaID)) +
  geom_bar(position = "stack", stat = "count") +
  facet_grid(~ season, switch = "x") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(-.01,"cm"))
#grass shrimp dominant throughout
#more diversity change in edge communities - higher presence of killifish vs. shrimp, 

# load tray data---
tray.env<-read.csv("wdata/environment data.csv")%>%
  mutate(sampling=paste(season,yr))
tray.env$season<-factor(tray.env$season,levels=c("Fall","Winter","Spring"))
tray.env$sampling<-factor(tray.env$sampling,levels=c("Fall 2025","Winter 2026","Spring 2026"))
tray.com<-read.csv("wdata/community data.csv")





# load oyster data---
oys.count<-crcl.trps.dr<-read_xlsx("odata/CRCL Oysters.xlsx",sheet = 2)
oys.size<-crcl.trps.dr<-read_xlsx("odata/CRCL Oysters.xlsx",sheet = 3)


# theme set for presentation plots
theme_set(theme_bw() + 
            theme(panel.grid = element_blank(),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size= 18),
                  plot.background = element_rect(fill = "#F0F0E0", color = NA),
                  panel.background = element_rect(fill = "#F0F0E0", color = NA),
                  strip.background = element_rect(fill = "#F0F0E0", color = NA),
                  legend.background = element_rect(fill = "#F0F0E0", color = NA),
                  text = element_text(family = "serif")))

ggplot(trp.lengths, aes(x = taxaID, y = length)) +
  geom_boxplot(fill = "#F0F0E0")



