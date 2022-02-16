# Tutorial Code CC Modelling 2 #

# Librairies ----

library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
library(lme4)  # for hierarchical models
library(sjPlot)  # to visualize model outputs
library(ggeffects)  # to visualize model predictions
library(MCMCglmm)  # for Bayesian models
library(MCMCvis)  # to visualize Bayesian model outputs
library(stargazer)  # for tables of model outputs

# Load and check data ----

toolik_plants <- read.csv("toolik_plants.csv")

head(toolik_plants)
str(toolik_plants)

toolik_plants <- toolik_plants %>%
  mutate(across(c(Site, Block, Plot), as.factor))

unique(toolik_plants$Site)
length(unique(toolik_plants$Site))  # we have 5 sites

toolik_plants %>% 
  group_by(Site) %>%  # group by site to get the nb of blocks in each site
  summarise(block.n = length(unique(Block))) 

toolik_plants %>% 
  group_by(Block) %>%  # group by block to get the nb of plots in each block
  summarise(plot.n = length(unique(Plot)))

unique(toolik_plants$Year)  # 4 years of data

length(unique(toolik_plants$Species))  # how many species identified
unique(toolik_plants$Species)  # shows that some species are not actually plants = rocks 

# filter out unwanted species 
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

length(unique(toolik_plants$Species))  # we can see it worked

# calculate species richness 
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species))) %>%
  ungroup()

# plot species richness in histogram 
(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

# histogram of plant cover
(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic())

# Before making model, need to consider ----
# Skewness of the data 
# Spatial autocorrelation 
# Temporal autocorrelation 
# if data mainly small values or not (?) 
# What is the research question + what are the dependent (response) and independent (predictor) variables ?

# General linear models ----
# without any random effect 

plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)  # year thing changed to years 1, 2, 3 and 4 instead of being treated as values = 2008
summary(plant_m)

# assumption that the data are normally distributed + data points independent from each other + relationship between variables is linear 
# our data doesn't fit those assumptions
# check residuals 
plot(plant_m)
# shows us that outliers are driving the results

# Hierarchical models ----

# effect of Year on richness ----
# if we stop looking at temporal differences OR plots OR block differences 
plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)
# estimate and fixed effects columns tell us the effect size = here year has an effect of about -0.7 on richness 

plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)  # adding block effect
summary(plant_m_plot2)

plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)  # adding plot effect
summary(plant_m_plot3)
# this last model tells us how richness has changed over time + accounts for the hierarchical structure of the data 

plot(plant_m_plot3)  # checking residuals 
# since points are eenly distributed along the line, residuals are ok

# Plot the results to see assumptions (visualize random effect)
set_theme(base = theme_bw() +  # make a theme
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))

save_plot(filename = "model_re.png", height = 8, width = 15)  # save plot in wd

# to visualize fixed effect = year (point is estimate of effect and line around the point is the confidence interval)
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))

save_plot(filename = "model_re.png", height = 8, width = 15)

# effect of mean T° on richness ----
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)
# hierarchical structure stays the same for site, block and plot = account for the random effect
# year accounts for the temporal replication = other type of random effect

# visualize the fixed effect
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))
# large uncertainty because wide confidence interval
save_plot(filename = "model_temp_fe.png",
          height = 8, width = 15)

# visualize the random effects of plot, block, site and finally year
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))
save_plot(filename = "model_temp_re.png",
          height = 8, width = 15)

# Random slopes versus random intercepts ----
# look at effect of mean T° on each plot 
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs)
# summary message tells us that model structure is too complicated for the data we have = need to simplify it 

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs)
# here the model works but not good because we are violating the assumption that data points are independent 
# by not taking into account the separate structure below site 

# visualize the results 
(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))

save_plot(filename = "model_plant_fe.png",
          height = 8, width = 15)

(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))

save_plot(filename = "model_plant_re.png",
          height = 8, width = 15)

# visualize model predictions
ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% 
  plot()

save_plot(filename = "model_temp_richness.png",
          height = 12, width = 14)

ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re") %>% plot() +
  theme(legend.position = "bottom")

save_plot(filename = "model_temp_richness_rs_ri.png",
          height = 12, width = 14)
# be careful here because relationship seems strong but y axis doesn't start at 0!! so relationship not that strong actually 

# plotting predictions for mean T° to overcome this scale problem 
predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))

(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    scale_y_continuous(limits = c(0, 35)) +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot1, filename = "overall_predictions.png",
       height = 5, width = 5)
# relationship not as strong anymore 

# do the same whilst including the random effect
predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re")

(pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_y_continuous(limits = c(0, 35)) +
    theme(legend.position = "bottom") +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot2, filename = "ri_rs_predictions.png",
       height = 5, width = 5)

# re-plot to make the axis start at 0 
(pred_plot3 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom") +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot3, filename = "ri_rs_predictions_zoom.png",
       height = 5, width = 5)

# Same hierarchical model but using generalized linear mixed-effects models ----
# using a Markov chain Monte Carlo approach under a Bayesian statistical framework

