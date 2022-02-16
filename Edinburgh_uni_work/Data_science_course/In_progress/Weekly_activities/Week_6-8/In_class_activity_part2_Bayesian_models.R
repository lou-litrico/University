# In-class activity For Weeks 6, 7 and 8
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
toolik_plants <- read_csv("data/toolik_plants.csv")


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
(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

(hist_plots <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    facet_wrap(vars(Plot)) +
    theme_classic())


# Week 6 Hierarchical Models: general linear models ----
# Richness over time
# What kind of a distribution are we using?
library(lmerTest)
plant_m <- lmer(Richness ~ I(Year-2007) + (1|Site),
                data = toolik_plants)
summary(plant_m)
plant_m
save(plant_m, file = "weekly_materials/week_06-08/model_output/plant_m.Rdata")

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

# Week 6 hierarchical models: Genneralised linear models ----
# What is the most appropriate distribution for these data?

plant_m2 <- glmer(Richness ~ I(Year - 2007) + (1|Site),
                  family = "poisson",
                  data = toolik_plants)
summary(plant_m2)
save(plant_m2, file = "weekly_materials/week_06-08/model_output/plant_m2.Rdata")

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
save(plant_m3, file = "weekly_materials/week_06-08/model_output/plant_m3.Rdata")

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


# Activity for Week 7 Bayesian Hierarchical Models ----
# Generalised linear model in a Bayesian framework

# Using MCMCglmm - This takes a long time! 10 min or more?
# plant_mMCMC <- MCMCglmm(Richness ~ year.scaled, data = toolik_plants,
#                        family = "poisson", nitt = 60000, burnin = 10000)
#
# save(plant_mMCMC, file = "weekly_materials/week_06-08/model_output/plant_mMCMC.Rdata")

load("weekly_materials/week_06-08/model_output/plant_mMCMC.Rdata")

summary(plant_mMCMC)

# In Stan through brms - default priors (weakly-informative) - This takes a long time! 10 min or more?
# plant_mbrms <- brm(Richness ~ year.scaled + (1|Site),
#             data = toolik_plants, family = poisson())
#
# save(plant_mbrms, file = "weekly_materials/Week_06-08/model_output/plant_mbrms.Rdata")

load("model_output/plant_mbrms.Rdata")

summary(plant_mbrms)

ggpredict(plant_mbrms, terms = c("year.scaled")) %>% plot()

plot(marginal_effects(plant_mbrms))

# In Stan through brms - setting priors and other parameters - This takes a long time! 10 min or more?
# prior

hier_prior <- c(set_prior(prior = 'normal(0,6)', class='b', coef='year.scaled'), 	# global slope
                set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
                set_prior(prior = 'cauchy(0,2)', class='sd'))							# group-level intercepts and slopes

# This takes a long time! 10 min or more?
# plant_mbrms2 <- brm(bf(Richness ~ year.scaled + (year.scaled|Site),
#                            family = brmsfamily('poisson')), data = toolik_plants,
#                         prior = hier_prior, iter = 2000,
#                         warmup = 500,
#                         inits = '0',
#                         control = list(adapt_delta = 0.80),
#                         cores = 2, chains = 2)
#
# save(plant_mbrms2, file = "weekly_materials/week_06-08/model_output/plant_mbrms2.Rdata")

load("model_output/plant_mbrms2.Rdata")

summary(plant_mbrms2)

ggpredict(plant_mbrms, terms = c("year.scaled")) %>% plot()

plot(conditional_effects(plant_mbrms2))

# Extract slopes for each cell
slopes_plants <- as.data.frame(coef(plant_mbrms2))

save(slopes_plants, file = "weekly_materials/week_06-08/model_output/slopes_plants.Rdata")
