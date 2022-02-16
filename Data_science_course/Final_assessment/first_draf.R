# Final assessment (tutorial) # 

# Libraries ----
library(tidyverse)
library(scales)

# Load and check data ----
LPI_data <- read_csv("LPI_data.csv")
head(LPI_data)  # 69 variables 
str(LPI_data)  # in wide format now so will need to change that into tidy data

# Initial data manipulation ----
LPI_long <- LPI_data %>% 
  gather(data = ., key = "year", value = "pop", 25:69) %>%  # turn into long format with one observation in each line
  filter(is.finite(pop)) %>%  # take out empty observations 
  mutate(year = parse_number(year)) %>%  # we need year as a numerical variable for our analysis
  group_by(id) %>%
  filter(length(unique(year)) > 5) %>%  # this only keeps studies that lasted more than 5 years 
  mutate(scalepop = rescale(pop, to = c(-1, 1))) %>%  
  # this previous line scales the pop values to be centered around 0 and only vary between 1 and -1 which will help our model
  drop_na(scalepop) %>%  # I don't think that there are any (since we did is.finite(pop) before)!!!
  ungroup()
# can take away the code that creates scalepop because won't use it I think 

length(unique(LPI_long$Country.list))  # lots of different locations included in this dataset, will need to refine 
unique(LPI_long$Class)  # also lots of different species observed so will subset for that as well 
# make that into the original question we are trying to answer with the model not the other way around 

France <- LPI_long %>% 
  filter(Country.list == "France", Common.Name == "Knot / Red knot")

North <- France %>% 
  filter((grepl("Channel", .$Location.of.population)))
hist(North$pop, breaks = 20)

West <- France %>% 
  filter((grepl("Atlantic", .$Location.of.population)))
hist(West$pop, breaks = 20)

unique(France$Location.of.population)
# data from only 2 location so will need to account for that as a random effect
# However the locations are really broad areas  so might not be very relevant
# = the whole of the west coast and the whole of the north coast of the country 

# Visualize the data ---
hist(France$pop, breaks = 20)  # our population follows a poisson distribution because it is count data 

# total distribution is uniform 
# BUT as we saw earlier the data is seperated by pot, type and pair 
# sand since we are going to ask our model to account for this structure, we need to give it the appropriate 
# data ditribution of that structure. 
# a general rule is to look at the distribution of the lowest structural level of the data that you will
# include in your model. 
# And is the distribution is different for some of your data points, just select a sub-sample of that data 
# which all have the same/similar distribution 

boxplot(pop ~ Location.of.population, data = France) 

boxplot(height ~ pair, data = maize)
boxplot(height ~ type, data = maize)

toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species)))

# Explore the distribution of the data
(hist2 <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())
