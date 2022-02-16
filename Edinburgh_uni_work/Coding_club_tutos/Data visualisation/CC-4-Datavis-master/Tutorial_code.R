# Data Visualization tutorial #

# Libraries ----

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Load and check LPI data ----

LPI <- read.csv("LPIdata_CC.csv")
str(LPI)

# Data wrangling to select variables + species of interest ----

LPI2 <- gather(LPI, "year", "abundance", 9:53)  # long format

LPI2$year <- parse_number(LPI2$year)
LPI2$abundance <- as.numeric(LPI2$abundance)

vulture <- LPI2 %>% 
  filter(Common.Name == "Griffon vulture / Eurasian griffon") %>% 
  na.omit()

# Create plots using ggplot ----

# Abundance of vultures with mean line ----
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +
    geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") +
    geom_vline(aes(xintercept = mean(abundance)), 
               colour = "red", linetype = "dashed", size=1) +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nGriffon vulture abundance") +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Abundance of vultures in Croatia and Italy by year + linear model fit ----
vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +   
    geom_smooth(method = "lm", aes(fill = Country.list)) +  # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +   # Adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#EE7600", "#00868B"),   # Adding custom colours for lines and points
                        labels = c("Croatia", "Italy")) +   # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title
          legend.position = c(0.9, 0.9)))     

# Abundance of vultures by country boxplot ----
(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) + 
    scale_colour_manual(values = c("#EE7600", "#00868B")) + 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))  # Removing legend - not needed with only 2 factors

# Richness of common names per country barplot ----
richness <- LPI2 %>% 
  filter (Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name)))) # create new column based on how many unique common names (or species) there are in each country 

(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Abundances of vultures every year in different countries ----
(vulture_scatter_all <- ggplot(vulture, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   

# BUT this is too much info = not visible 
# so change it intp panels 

(vulture_scatter_facets <- ggplot(vulture, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    facet_wrap(~ Country.list, scales = "free_y") +                      # THIS LINE CREATES THE FACETTING + free scales to allow different abundance scales per country
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   

# Make a pannel of all graphs ----
grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)  # looks bal and streched out

# make it better 
(panel <- grid.arrange(
  vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.85, 0.85)), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have

# save panel 
ggsave(panel, file = "vulture_panel2.png", width = 5, height = 12) 

# Challenge ----

# select 2 species over time from LPI 

species <- LPI %>% 
  gather(data = ., "year", "abundance", 9:53) %>% 
  mutate(year = parse_number(.$year), abundance = as.numeric(.$abundance)) %>% 
  filter(Common.Name == c("Grey seal","Sea otter")) %>% 
  na.omit()

# Abundance of gray seal and sea otter over time 
(species_scatter <- ggplot(species, aes (x = year, y = abundance, colour = Common.Name)) +
    geom_point(size = 2) +   
    geom_smooth(method = "lm", aes(fill = Common.Name)) +  # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +   # Adding custom colours for solid geoms (ribbon)
    ylab("Sea otter and Gray seal aundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title
          legend.position = c(0.9, 0.9)))     

# Abundance of sea otter and gray seal in 5 countries 

countries <- filter(species, Country.list %in% c("United Kingdom", "Canada", "United States", "Russian Federation", "Iceland"))

(countries_barplot <- ggplot(countries, aes(x = Country.list, y = abundance)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    facet_wrap(~ Common.Name, scales = "free_y") +
    theme_bw() +
    ylab("Abundance\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
