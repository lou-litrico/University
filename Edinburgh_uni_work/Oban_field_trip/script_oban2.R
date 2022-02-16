# OBAN GROUP PROJECT #
# DATA ANALYSIS AND PRESENTATION #

# Script prep ----

setwd("~/Desktop/R- Files")

# Libraries 

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(vegan)
library(wesanderson)
library(devtools)
library(pairwiseAdonis)

# Data frame cleanup ----

invert <- read.csv("oban_data.csv") %>% 
  select(-Dermaptera) %>%  # Because we didn't have any
  replace(is.na(.), 0) %>% 
  rename(Distance = Distance.from.road..m.) %>%
  rename(Road.type = Site.type) %>% 
  mutate(Transect_id = paste0("S",Site,"T",Transect)) %>% 
  mutate(Distance = as.factor(Distance),
         Transect = as.factor(Transect),
         Site = as.factor(Site),
         Transect_id = as.factor(Transect_id))

head(invert)
str(invert)

# Total counts barplot ----
# There is probably a better/quicker way to do this but I don't know it 

total_counts <- c(sum(invert$Acari),sum(invert$Annelida),
                  sum(invert$Araneae), sum(invert$Coleoptera),
                  sum(invert$Collembola),sum(invert$Diptera),
                  sum(invert$Gastropoda),sum(invert$Hemiptera),
                  sum(invert$Hymenoptera),sum(invert$Isopoda),
                  sum(invert$Larvea),sum(invert$Thysanoptera),
                  sum(invert$Symphyla),sum(invert$unknown))
Orders <- c("Acari","Annelida","Araneae","Coleoptera","Collembola",
            "Diptera","Gastropoda","Hemiptera","Hymenoptera",
            "Isopoda","Larvea","Thysanoptera","Symphyla","Unknown")
totals <- data.frame(total_counts, Orders)

(totals_barplot <- ggplot(totals) +
    geom_col(aes(x = Orders, y = total_counts), fill = "#a9c3e1", colour = "black") +
  ylab("Total individuals count\n") +
  xlab("\nInvertebrate orders") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, size = 15, vjust = 1, hjust=1),
        axis.title = element_text( size = 15),
        axis.line = element_line(colour = "black")))

# Diversity index  ----

invert %>% 
  select(-c("Road.type","Site","Transect","Distance","Transect_id")) %>% 
  as.matrix() -> invert_mat

# Adding new colums with the diversity indices?
invert <- mutate(invert, Shannons = diversity(invert_mat), 
                  Simpsons = diversity(invert_mat, index = "simpson"))

write.csv(invert, "diversity.csv")

# Shannon's index = boxplot along distance colored by road type ----

(distance_boxplot_shannons <- ggplot(invert, aes(Distance, Shannons, fill = Road.type)) +
    geom_boxplot()+
    theme_bw() +
    scale_fill_manual(values = c("#ddffa2", "#a9c3e1"), 
                      name = "Road type", 
                      labels=c('Double track', 'Single Track')) +
    ylab("Shannon's Diversity Index\n") +                             
    xlab("\nDistance from the road (m)")+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                            
          plot.margin = unit(c(1,1,1,1), units = , "cm")))  

# Simpson's index = boxplot along distance colored by road type ----

(distance_boxplot_simpsons <- ggplot(invert, aes(Distance, Simpsons, fill = Road.type)) +
    geom_boxplot()+
    theme_bw() +
    scale_fill_manual(values = c("#ddffa2", "#a9c3e1"), name = "Road type", 
                      labels=c('Double track', 'Single Track')) +
    ylab("Simpson's Diversity Index (1-D)\n") +                             
    xlab("\nDistance from the road (m)")+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                            
          plot.margin = unit(c(1,1,1,1), units = , "cm")))  

# Other ugly plots (not used in report) ----

#site 
(site_boxplot_simpsons <- ggplot(invert, aes(Site, Simpsons, fill = Road.type)) +
    geom_boxplot())

#site type
(site_boxplot_simpsons <- ggplot(invert, aes(Road.type, Simpsons, fill = Road.type)) +
    geom_boxplot())

# Stats for diversity indices ----

# Shannon's ANOVA #

shannons_model <- lm(Shannons ~ Road.type*Distance, data = invert)

summary(shannons_model)
anova(shannons_model)
par(mfrow=c(2,2))
plot(shannons_model)

shannons_model2 <- lm(Shannons ~ Distance + Road.type, data = invert)

summary(shannons_model2)
anova(shannons_model2)
plot(shannons_model2)

site_type_sh <- lm(Shannons ~ Road.type, data = invert)

summary(site_type_sh)
anova(site_type_sh)
plot(site_type_sh)

# Simpson's ANOVA #

simpsons_model <- lm(Simpsons ~ Site*Distance, data = invert)

summary(simpsons_model)
anova(simpsons_model)
plot(simpsons_model)

simpsons_model2 <- lm(Simpsons ~ Distance + Site, data = invert)

summary(simpsons_model2)
anova(simpsons_model2)
plot(simpsons_model2)

site_type_sim <- lm(Simpsons ~ Site, data = invert)

summary(site_type_sim)
anova(site_type_sim)
plot(site_type_sim)

# NMDS ----

# change NA to 0 again
#invert[invert == 0] <- NA

# To check how many dimensions you need

glimpse(invert)
invert <- invert[!(invert$Transect_id == "S3T1" & invert$Distance == "6"),]

invert.NMDS <- metaMDS(invert [,5:18], distance = "bray", k = 8, trymax=300)

par(mfrow=c(1,1))
invert.NMDS$stress

# NMDS plot by distance (using BASE R) ----

# create group of colours for the communities (distances) #

group <- as.character(invert$Distance)
colours <- as.character(invert$Distance)

as.character(invert$Distance) %>% 
  replace(colours=="0", "#ddffa2") %>% 
replace(colours=="6", "#a9c3e1") %>% 
replace(colours=="12", "#ff4f4f") %>% 
replace(colours=="18", "#fffcbf") -> colours

# create the NMDS plot #

par(mfrow=c(1,1))
ordiplot(invert.NMDS, type = "n", cex.axis = 2, cex.lab=2)

for(i in unique(group)) {
  ordihull(invert.NMDS$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colours[grep(i,group)],label=F) } 

orditorp(invert.NMDS, display = "species", col = "red", air = 0.01)
orditorp(invert.NMDS, display = "sites", label=F, air = 0.01, cex = 1.25)
legend('bottomright', legend=c("0m","6m","12m","18m"), col=unique(colours), 
       title = "Distance from road", bty = "n", pch = 16)
legend('bottomleft', legend="stress = 0.042", bty = "n")

# NMDS plot by road types ----

# create different group of colours for communities (road types) #

group <- as.character(invert$Road.type)
colours <- as.character(invert$Road.type)

as.character(invert$Road.type) %>% 
  replace(colours=="Small", "#ddffa2") %>% 
  replace(colours=="Road", "#a9c3e1") -> colours

# create the NMDS plot # 

ordiplot(invert.NMDS, type = "n", cex.axis = 2, cex.lab=2)

for(i in unique(group)) {
  ordihull(invert.NMDS$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colours[grep(i,group)],label=F) } 

orditorp(invert.NMDS, display = "species", col = "red", air = 0.01)
orditorp(invert.NMDS, display = "sites", label=F, air = 0.01, cex = 1.25) 
legend('bottomright', legend=c("Single track","Double track"), col=unique(colours), 
       title = "Road type", bty = "n", pch = 16)
legend('bottomleft', legend="stress = 0.042", bty = "n")

# PERMANOVA for NMDS ----

# multivariate analysis of variance used to compare groups of objects

(invert.fit <- adonis(invert[,5:18] ~ Distance*Site, invert, 
                      permutations = 999, method = "bray"))

pairwise.adonis(invert[,5:18], invert$Site)  # post hoc test 

# Abundance against road type  boxplot (not used in report) ----

invert <- mutate(invert, abundance = rowSums(invert[c(5:18)]))

(abundance_boxplot <- ggplot(invert, aes(Road.type, abundance)) + 
   geom_boxplot(aes(fill = Road.type)) +
   theme_bw() +
   scale_fill_manual(values = c("#ddffa2", "#deebf7")) +   
   ylab("Abundance of invertebrates\n") +                             
   xlab("\nRoad type")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),                                     
         plot.margin = unit(c(1,1,1,1), units = , "cm")))     

# Abundance against site  boxplot ----

(abundance_boxplot <- ggplot(invert, aes(Site, abundance, fill = Road.type)) +
    geom_boxplot() +
    theme_bw() +
    scale_fill_manual(values = c("#ddffa2", "#a9c3e1"), name = "Road type", 
                                 labels=c('Double track', 'Single Track')) +  
    ylab("Abundance of invertebrates\n") +
    xlab("\nSite")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Stats for abundance ----

abundance_anova <- aov(abundance ~ Site, data = invert)

anova(abundance_anova)
summary(abundance_anova)

TukeyHSD(abundance_anova)

par(mfrow=c(2,2))
plot(abundance_anova)

abundance_resids <- resid(abundance_anova)
shapiro.test(abundance_resids)
bartlett.test(abundance ~ Road.type, data = invert)

 # Vegetation density ----

vegdensity <- read.csv("diversity_density.csv")%>% 
  select(-Shannons) %>% 
  select(-Simpsons) %>% 
  select(-Transect_id)

Density <- cbind(invert, vegetation_density)

# Scatter plots against diversity colored by Road type ----

# Shannon's index

(vegdensity_scatter <- ggplot(Density, ) + 
    geom_point(aes(x = vegetation_density, y = Shannons, colour = Road.type), size = 3) +
    ylab("Shannon's diversity index\n") +
    xlab("\nVegetation density") +
    scale_colour_manual(values = c("#ddffa2", "#a9c3e1"), name = "Road type", 
                        labels=c('Double track', 'Single Track')) +
    theme_bw() +
    theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.line = element_line(colour = "black")))

# Simpson's index

(vegdensity_scatter <- ggplot(Density) + 
    geom_point(aes(x = vegetation_density, y = Simpsons, colour = Road.type), size = 3) +
    ylab("Simpson's diversity index (1-D)\n") +
    xlab("\nVegetation density") +
    scale_colour_manual(values = c("#ddffa2", "#a9c3e1"), name = "Road type", 
                        labels=c('Double track', 'Single Track')) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust=1),
          axis.line = element_line(colour = "black")))

# Scatter plots against diversity colored by distance ----

# Shannon's index

(vegdensity_scatter <- ggplot(Density) + 
    geom_point(aes(x = Density$vegetation_density, y = Shannons, colour = Distance), 
               size = 3) +
    ylab("Shannon's diversity index\n") +
    xlab("\nVegetation density") +
    scale_colour_manual(values = c("#ddffa2","#a9c3e1","#ff4f4f","#fffcbf"), 
                        name = "Distance from the road", 
                        labels=c('0m', '6m', '12m', '18m')) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust=1),
          axis.line = element_line(colour = "black")))

# Simpson's index

(vegdensity_scatter <- ggplot(Density) + 
    geom_point(aes(x = Density$vegetation_density, y = Simpsons, colour = Distance), 
               size = 3) +
    ylab("Simpson's diversity index (1-D)\n") +
    xlab("\nVegetation density") +
    theme_bw() +
    scale_colour_manual(values = c("#ddffa2","#a9c3e1","#ff4f4f","#fffcbf"), 
                        name = "Distance from the road", 
                        labels=c('0m', '6m', '12m', '18m')) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust=1),
          axis.line = element_line(colour = "black")))

# Stats for vegetation density (ANOVA) ----

# Shannon's 

vegdensity_anova <- aov(Shannons ~ vegetation_density, data = Density)
summary(vegdensity_anova)
plot(vegdensity_anova)

# Simpson's 

vegdensity_anova2 <- aov(Simpsons ~ vegetation_density, data = Density)
summary(vegdensity_anova2)
plot(vegdensity_anova2)
