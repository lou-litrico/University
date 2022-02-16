# Challenge 1 new code # 

# Libraries ----
library(dplyr)      # data manipulation (part 2, 3 and 5)
library(readr)      # parse_number function used in part 2
library(tidyr)      # tidying data into long format (part 2)
library(ggplot2)    # for plots (part 4 and 6)
library(ggthemes)   # to get the map background in part 6
library(gridExtra)  # to make a panel with 2 graphs at the end

# 1 - Load and check Living Planet Data ----

LPI_data <- read.csv("Data/LPI_birds.csv")

head(LPI_data)
str(LPI_data)

# 2 - Create dataframe with desired columns ----

# 1) Shift into long format data 
# 2) Change year into a numerical variable 
# 3) Create a genus_species_id column
# 4) Take out NA values in the pop column
# 5) Create new colum with timespan of the observation and another with place in range (???)
# 6) Remove non-numerical values and length values under 5 
# 7) Remove citation and authority columns

LPI_long <- LPI_data %>% 
  pivot_longer(data = ., cols=25:69, names_to = "year",values_to = "pop") %>% 
  mutate(year = parse_number(.$year),
         genus_species_id = paste(.$Genus, .$Species, .$id, sep = "_")) %>%  
  filter(!is.na(pop)) %>%   
  group_by(genus_species_id) %>%  # to get the max and min years specific to each genus_species_id instead of those of the whole dataset
  mutate(lengthyear = (max(year) - min(year)),
         scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>% 
  filter(is.finite(scalepop), lengthyear > 5) %>%  
  ungroup() %>% 
  select(-Data.source.citation, -Authority)

# 3 - Create dataframe for UK arquata observations ----

# 1) Only keep the arquata observations from the UK that lasted more than 15 years 
# 2) Select variables of interest 

curlew <- LPI_long %>% 
  filter(Species == "arquata", 
         Country.list == "United Kingdom", 
         lengthyear > 15) %>% 
  select(Country.list, year, scalepop, id, lengthyear)

# 4 - Plot curlew pop trends in the UK ----

(curlew_plot <- ggplot(curlew, aes(x = year, y = scalepop, group = id, colour = Country.list)) + 
    geom_line() +                                              # add lines to link the datapoints 
    geom_point() +                                             # add points on top of the lines  
    theme_classic() +                                          # white background and black axis lines
    theme(legend.position = "none") +                          # no legend for the color of the line
    labs(title="Curlew trends in the UK") +                    # add title
    theme(plot.title = element_text(size = 15, hjust = 0.5)))  # title position and size

# 5 - Load and check Site Coordinate Data ----

site_coords <- read.csv("site_coords.csv")

head(site_coords)
str(site_coords)

# Merge curlew data with site coordinates
curlew_coord <- left_join(curlew, site_coords, by = "id")  

# 6 - Map of curlew positions ---- 

(curlew_map <- ggplot(curlew_coord, aes(x = Decimal.Longitude, y = Decimal.Latitude, colour = Country.list)) +
   borders("world", colour = "gray40", fill = "gray75", size = 0.3) +
   coord_cartesian(xlim = c(-10, 35), ylim = c(30, 70)) +
   theme_map() +
   geom_point(size=4) +
   theme(legend.position="none") +
   theme(plot.title=element_text(size=15, hjust=0.5)) +
   labs(title="Curlew population map"))

# Make a panel of the two graphs
curlew_results <- grid.arrange(curlew_plot, curlew_map, ncol = 2)

# Save it in working directory
ggsave(filename = "curlew-plots-panel.png", curlew_results, device = "png", height = 12, width = 16, units = "cm")

