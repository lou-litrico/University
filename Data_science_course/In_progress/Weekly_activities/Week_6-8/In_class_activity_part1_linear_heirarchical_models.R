# In-class activity For Weeks 6 and 7
# Started off by Gergana Daskalova, added to by Isla Myers-Smith
# 28th Oct 2020

# Libraries ----
library(tidyverse)
library(lme4)
library(brms)
library(MCMCglmm)
library(broom)
library(broom.mixed)
library(ggeffects)

# Toolik data
toolik_plants <- read_csv("weekly_materials/week_06-08/data/toolik_plants.csv")

# Inspect data
head(toolik_plants)
str(toolik_plants)

toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

length(unique(toolik_plants$Site))

unique(toolik_plants$Year)

# Remove non-species
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

# Calculate species richness
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species)))

# Explore the distribution of the data
(hist2 <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())


# Week 6 general linear models ----
# Richness over time
# What kind of a distribution are we using?
library(lmerTest)
plant_m <- lmer(Richness ~ I(Year-2007) + (1|Site),
                 data = toolik_plants)
summary(plant_m)
plant_m

# Updated to use the package broom.mixed
model.table <- as.data.frame(tidy(plant_m))

# The species richness of plants in the Arctic has decreased over the past 10 years
# at a rate of 0.7 species per year (Figure 1, slope = -0.70 +/- 0.01, Table S12)
# sample size = plots = ?, sites = 5, p).

# What does the model output say?
# Does the model output change if you run the model again?
# What values should you report?
# What is the error around this model? Are you confident in it?
# How is species richness changing over time?
# What is a model prediction?
# What is the predicted species richness in the year 2013?

# Genneralised linear models ----
# What is the most appropriate distribution for these data?

plant_m2 <- glmer(Richness ~ I(Year - 2007) + (1|Site),
                 family = "poisson",
                 data = toolik_plants)
summary(plant_m2)

# Oh no, the model hasn't converged!!!
# What do we do?
# Do we panic? Do we quit?
# What does the error message say?
# How should we proceed?

# What does scaling the year variable do?
toolik_plants$year.scaled <- scale(I(toolik_plants$Year - 2007), center = T)

# Let's try our model again!
plant_m3 <- glmer(Richness ~ year.scaled + (1|Site),
                  family = "poisson",
                  data = toolik_plants)
summary(plant_m3)

ggpredict(plant_m3, terms = c("year.scaled")) %>% plot()

predictions <- ggpredict(plant_m3, terms = c("year.scaled"))

toolik_plants_simple <- toolik_plants %>% dplyr::select(Richness, year.scaled) %>%
  distinct()

ggplot() +
  geom_line(data = predictions, aes(x = x, y = predicted),
            size = 2) +
  geom_ribbon(data = predictions, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.1) +
  geom_point(data = toolik_plants_simple, aes(x = year.scaled, y = Richness),
             alpha = 0.1, size = 2) +
  annotate("text", x = -0.65, y = 5, label = "Slope = -0.06, Std. error = 0.01") +
  scale_y_continuous(limits = c (0, 50)) +
  theme_classic() +
  labs(x = "\nYear (scaled)", y = "Species richness\n")

# What does the model output say?
# What values should you report?
# What is the error around this model? Are you confident in it?
# How is species richness changing over time?
# What is the predicted species richness in the year 2013?

# Does the output of the models differ depending on what distribution you use?
# Which model do you trust more?
