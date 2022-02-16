# Challenge 3 Statistical Modelling ----
# Data Science in EES 2021
# Script written by Louise Litrico
# 6th November

# Libraries ----
library(tidyverse)
library(brms)
library(ggeffects)
library(tidybayes)
library(modelr)

# Load Living Planet Data, select species, and start data manipulation ----
load("data/LPI_species.Rdata")

fox_data <- LPI_species %>% 
  filter(Common.Name == "Red fox") %>% 
  mutate(year = as.character(.$year)) %>% 
  mutate(year = parse_number(.$year)) %>%  # make year as numerical values
  mutate(origin = case_when(grepl("Yes", .$Native) ~ "Native", 
                            grepl("Yes", .$Invasive) ~ "Invasive")) %>% 
  select(-Native, -Invasive, -Alien) %>%  # reduce to 1  column for origin instead of 3 
  group_by(Data.source.citation) %>% 
  mutate(lengthyear = (max(year) - min(year))) %>%  # look at length of studies
  ungroup()

write_csv(fox_data, "data/fox_data.csv")  # because Rmd won't run the above code 

# Explore data to find relevant questions ----
str(fox_data)
head(fox_data)
names(fox_data)

length(unique(fox_data$Sampling.method))  # 10 sampling methods (1 for each study)
unique(fox_data$Units)  # need to chose those that have similar units to be able to compare 

# Subset for data from track counts over a day ----
track <- fox_data %>%  
  filter(Units %in% c("Track index - the number of crossings per 24h per 10km.",
                      "Red fox track trails per 1km per day",
                      "Track density (10km<8c><bf><8c>_ 24h <8c><bf><8c>_ )",
                      "Tracks per 100m transect per 24 recordings"))

# Check data structure and distribution ----
track %>% 
  group_by(Country.list) %>% 
  summarise(location.n = length(unique(Location.of.population))) %>% 
  ungroup()  # so Belarus has 2 locations sampled and Finland has 5

hist(track$pop, breaks = 40)

# Hierarchical linear model ----
track_mbrms <- brms::brm(pop ~ I(year - 1970) + (I(year - 1970)|Country.list/Location.of.population), 
                          data = track, family = exponential(), chains = 3, 
                          iter = 3000, warmup = 1000, prior = prior1)

# save(track_mbrms, file = "model_output/track_mbrms.Rdata")
load("model_output/track_mbrms.Rdata")

# Model results (tables)----
p <- summary(track_mbrms)
df <- data.frame(p$fixed) %>% select(-Bulk_ESS, -Tail_ESS)

slopes_track_countries <- as.data.frame(coef(track_mbrms)$Country.list)
slopes_countries_transpose <- t(slopes_track_countries)

slopes_track_locations <- as.data.frame(coef(track_mbrms)[2])
slopes_location_transpose = t(slopes_track_locations)

# Getting the default priors ----
default_priors <- data.frame(prior_summary(track_mbrms, all = FALSE)) %>% 
  select(-coef, -group, -resp, -dpar, -nlpar, -bound)

# Model outputs and data visualization ----
plot(conditional_effects(track_mbrms))  # model plot
plot(track_mbrms)  # shows model converged well 
pp_check(track_mbrms, plotfun = "stat", stat = "mean")  # shows model fits data well 

# Graph with model + CI + raw data 
(model_fit <- track %>%
    add_predicted_draws(track_mbrms) %>%
    ggplot(aes(x = year, y = pop)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 1/2, colour = "black") +
    geom_point(data = track, colour = "darkseagreen4", size = 3) +
    scale_fill_brewer(palette = "Greys") + 
    ylab("Red fox abundance\n") +
    xlab("\nYear") +
    theme_bw() + 
    theme(legend.title = element_blank(),
          legend.position = c(0.85, 0.85)))
  
# Graph with country specific fits and raw data 
track %>%
  group_by(Country.list) %>%
  add_epred_draws(track_mbrms) %>%
  ggplot(aes(x = year, y = pop, color = ordered(Country.list))) +
  stat_lineribbon(aes(y = .epred), alpha = 0.5) +
  geom_point(data = track) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  ylab("Red fox abundance\n") +
  xlab("\nYear") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_discrete(name = "Location")

# Spaghetti graph per location 
track %>%
  group_by(Location.of.population) %>%
  add_epred_draws(track_mbrms, ndraws = 100) %>%
  ggplot(aes(x = year, y = pop, color = ordered(Location.of.population))) +
  geom_line(aes(y = .epred, group = paste(Location.of.population, .draw)), alpha = .1) +
  geom_point(data = track) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Location")
  
# Graph with model + data + CI colored by country
track %>%
  group_by(Country.list) %>%
  add_predicted_draws(track_mbrms) %>%
  ggplot(aes(x = year, y = pop, color = ordered(Country.list), fill = ordered(Country.list))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(data = track) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

# Graph for country specific posterior distributions + mean + CI 
track_mbrms %>%  
  spread_draws(b_Intercept, r_Country.list[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_Country.list) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye() +
  theme_bw()

# Graph for Location specific posterior distribution + mean + CI 
means = track %>%
  add_epred_draws(track_mbrms)

preds = track %>%
  add_predicted_draws(track_mbrms)

track %>%
  ggplot(aes(y = Location.of.population, x = pop)) +
  stat_interval(aes(x = .prediction), data = preds) +
  stat_pointinterval(aes(x = .epred), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()

# Trying out the online tidy output things ----
get_variables(track_mbrms)

track_mbrms %>%  # table 
  spread_draws(r_Country.list[condition,term]) %>%
  head(10)

track_mbrms %>%  # table 
  spread_draws(r_Country.list[condition,]) %>%
  summarise_draws()

track_mbrms %>%  # graph per country with mean + CI
  spread_draws(b_Intercept, r_Country.list[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_Country.list, .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() +
  theme_bw()

track_mbrms %>%  # graph for country posterior distribution
  spread_draws(b_Intercept, r_Country.list[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_Country.list) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

# This takes to long now but looked good when I managed to make it work (I think)
track_mbrms %>%
  spread_draws(b_IyearM1970, r_Country.list[condition,]) %>%
  mutate(condition_mean = b_IyearM1970 + r_Country.list) %>%
  ggplot(aes(y = condition, x = condition_mean, fill = stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  theme_bw()

track %>%
  add_epred_draws(track_mbrms, dpar = c("mu")) %>%
  sample_draws(50) %>%
  ggplot(aes(y = Location.of.population)) +
  stat_dist_slab(aes(dist = "norm", arg1 = mu), 
                 slab_color = "gray65", alpha = 1/10, fill = NA) +
  geom_point(aes(x = pop), data = track, shape = 21, fill = "#9ECAE1", size = 2)

track %>%
  add_epred_draws(track_mbrms) %>%
  ggplot(aes(x = .epred)) +
  stat_dotsinterval(quantiles = 100)