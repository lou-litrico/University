# In class activity for Data Science in EES 2021
# Starter script written by Isla Myers-Smith and Gergana Daskalova
# 21st October 2020 and 20th October 2021

# Instructions ----

# Each group will try to make one beautiful version and one ugly version of the same figure. To make your figure you need to use a pipe and dplyr functions and the ggplot2 package (all found in the TidyVerse). You can also use other functions. Here is a list of the dplyr functions:

# https://dplyr.tidyverse.org/reference/

# Before you start coding, you need to draw a work flow diagram on paper or a white board for how you are going to design your code. Then you need to take a photo of that diagram and upload it to the folder for this activity.

# Then you need to translate your workflow diagram into comments in the R code.

# Then you can start coding.

# Remember that you are working on this script collaboratively so commit, pull and push frequently!


# Starter code ----

# Libraries
library(tidyverse)
library(scales)
library(viridis)

# Load Living Planet Data
LPI_data <- read.csv("LPI_data.csv")

# Reshape data into long form
LPI_long <- gather(data = LPI_data, key = "year", value = "pop", 25:69) %>%
  filter(is.finite(pop)) %>%
  group_by(id) %>%
  filter(length(unique(year)) > 5) %>%
  mutate(scalepop = rescale(pop, to = c(-1, 1))) %>%
  drop_na(scalepop) %>%
  ungroup()

str(LPI_long)

# Calculate slopes of population change
LPI.models <- LPI_long %>%
  group_by(biome, Class, id) %>%
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(.,
         slope = summary(mod)$coeff[2]) %>%
  ungroup() %>%
  mutate(id = id,
         biome = biome,
         Class = Class)

# You can ignore the warnings, it's just because some populations don't have enough data

# Group activity ----
# Rank all of the biomes from most to least well represented
# with number of populations monitored

### Challenge part 1: Adapt that code to make that first figure more beautiful and save!

LPI_Bar <- LPI.models %>%
  group_by(biome) %>%
  summarise(count = length(unique(id))) %>%
  arrange(desc(count))

(LPI_Bargraph <- ggplot(data = LPI_Bar %>% 
                          mutate(biome = fct_reorder(biome, count, .desc = TRUE))) +
    geom_bar(aes(x = biome, y=count, fill = biome), stat="identity") +
    theme_classic() +
    scale_fill_viridis_d(option = "viridis", direction = -1) +
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1))) +
    theme(axis.text.x = element_text(hjust = 1, angle = 45), 
          legend.position = "blank", 
          axis.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
    labs(title = "\nNumber of Populations per Biome\n", x = "\nBiome\n", y = "\nNumber of populations\n"))

ggsave(LPI_Bargraph, filename = "bargraph.pdf", width = 10, height = 8)

# Add + beautify graph
# for all one color, put it out of the eaesthetic but for gradient, put it in the aes()
# color = ... will just color the outline, fill = ... will color the inside 
### Challenge part 2: Answer the second question with your own data wrangling and a beautiful graph and save!

# How are populations changing across the six best monitored biomes?

 biome_trends <- LPI.models %>% 
   filter(biome %in% c("Unknown", "Temperate broadleaf and mixed forests", "Boreal forests/taiga", "Temperate coastal rivers", "Temperate floodplain rivers and wetlands", "Temperate coniferous forests"))

 (biome_slope <- ggplot(biome_trends) +
     geom_histogram(aes(x = slope, fill = biome)) + 
     theme_classic() + 
     scale_fill_viridis_d(option = "viridis", direction = -1) +
     theme(legend.position = "blank") +
     facet_wrap(~ biome, scales = "free_y"))

 ggsave(biome_slope, filename = "bargraph_2.pdf", width = 10, height = 8)
 
 # HINT: You can use facet_wrap() or facet_grid() from the ggplot2 package
# to quickly create a graph with multiple panels

# Make your figure as beautiful as it can be and save that file

# Make another version that is as ugly as it can be and save that file

# Compare your figures back in plenary with the other groups

# The group with the prettiest and ugliest figures win the respective "glory"
