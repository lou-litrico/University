# Challenge 2: BBC Pitch - Visualizing inequalities females face in education (Afghanistan)
# Group 5: Ailsa, Ana, Barbora, David, Helene, Louise
# Data source: https://www.education-inequalities.org/countries/afghanistan#dimension=%7B%22id%22%3A%22sex%22%2C%22filters%22%3A%5B%5D%7D&year=%222015%22

# Libraries ----

library(tidyverse)
library(ggplot2)  
library(dplyr) # for Ailsa or if you can't use tidyverse
library(tidyr)
library(cowplot)

# Load and check data ----

data <- read.csv("raw_dataset/WIDE_2021-01-28_v1.csv")  # loads data on education indicators in Afghanistan (2015)
str(data)  # structure of data 

# David's work = create custom color palette and theme ----
# Creating custom themes
david_style <- function(){
  font <- "Helvetica"
  theme(plot.title = element_text(family = font, size = 14, face = "bold", color = "#222222", hjust = 0.5), 
        plot.subtitle = element_text(family = font, size = 12, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.position = "none",
        legend.text.align = 0, 
        legend.title = element_blank(), 
        legend.key = element_blank(), 
        legend.text = element_text(family = font, size = 9, color = "#222222"),
        axis.text = element_text(family = font, size = 9, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 12, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "#cbcbcb"), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 12, hjust = 0))
}


# Louise's work = Data wrangling before making the graphs ----

AFG_data <- data %>% 
  filter(country == "Afghanistan")  %>%  # selecting only AFG data
  select(-iso_code,  #  column name might also be ï..iso_code depending on laptop (change the code accordingly)
         -region_group, -income_group, -country, -grade, -level,  
         -slevel4_no, -slevel4_m, -rlevel4_no, -rlevel4_m, -mlevel4_no, 
         -mlevel4_m, -slevel3_no, -slevel3_m, -rlevel3_no, -rlevel3_m, 
         -mlevel3_no, -mlevel3_m, -slevel2_no, -slevel2_m, -rlevel2_no, 
         -rlevel2_m, -mlevel2_no, -mlevel2_m, -slevel1_no, -slevel1_m, 
         -rlevel1_no, -rlevel1_m, -mlevel1_no, -mlevel1_m, -eduout_lowsec_no,
         -eduout_prim_no, -eduout_upsec_no) %>%  # taking away columns that don't matter anymore OR don't have any data
  filter(!grepl("MICS", survey))  # only selecting the recent survey

# Making new object for sex comparison graph ----

AFG_data_sex <- AFG_data %>% 
  filter(category == "Sex")  %>%  # selecting sex differences 
  select(-Location, -Wealth, -Region, -Ethnicity, -Religion, -Language, -category) %>% 
  pivot_longer(data =., cols = 4:46, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator)) %>% 
  mutate(age_group = case_when(  # create age column with info from indicators 
    grepl("1822", indicator) ~ "18-22", 
    grepl("2529", indicator) ~ "25-29", 
    grepl("3034", indicator) ~ "30-34", 
    grepl("1524", indicator) ~ "15-24", 
    grepl("2029", indicator) ~ "20-29", 
    grepl("v2", indicator) ~ "3-4 years after graduation age", 
    grepl("edu0_prim", indicator) ~ "9-12", 
    grepl("2024", indicator) ~ "20-24", 
    grepl("eduout_lowsec", indicator) ~ "adolescent", 
    grepl("eduout_prim", indicator) ~ "children", 
    grepl("eduout_upsec", indicator) ~ "youth", 
    grepl("overage2plus", indicator) ~ "childen", 
    grepl("preschool_1ybefore", indicator) ~ "children", 
    grepl("preschool_3", indicator) ~ "3-4", 
    grepl("trans_lowsec", indicator) ~ "youth",  
    grepl("trans_prim", indicator) ~ "adolescent")) %>% 
  mutate(indicator_name = case_when(  # new indicator column to be more informative 
    grepl("attend_higher_1822", indicator) ~ "Attending higher education (18-22)", 
    grepl("comp_higher_2yrs_2529_m", indicator) ~ "Completed 2 years of higher education (25-29)",
    grepl("comp_higher_4yrs_2529_m", indicator) ~ "Completed 4 years of higher education (25-29)",
    grepl("comp_higher_4yrs_3034_m", indicator) ~ "Completed 4 years of higher education (30-34)",
    grepl("comp_lowsec_1524_m", indicator) ~ "Completed lower secondary school (15-24)",
    grepl("comp_lowsec_v2_m", indicator) ~ "Completed lower secondary school (3-5 years above graduation age)",
    grepl("comp_prim_1524_m", indicator) ~ "Completed primary school (15-24)", 
    grepl("comp_prim_v2_m", indicator) ~ "Completed primary school (3-5 years above graduation age)",
    grepl("comp_upsec_2029_m", indicator) ~ "Completed upper secondary school (20-29)",
    grepl("comp_upsec_v2_m", indicator) ~ "Completed upper secondary school (3-5 years above graduation age)",
    grepl("edu0_prim", indicator) ~ "Never attended school (9-12)",
    grepl("edu2", indicator) ~ "Less than 2 years of education (20-24)",
    grepl("edu4", indicator) ~ "Less than 4 years of education (20-24)", 
    grepl("eduout_lowsec", indicator) ~ "Out of school lower secondary adolescents",  
    grepl("eduout_prim", indicator) ~ "Out of school primary children",  
    grepl("eduout_upsec", indicator) ~ "Out of school upper secondary youth", 
    grepl("eduyears", indicator) ~ "Mean years of education", 
    grepl("literacy", indicator) ~ "Youth literacy rate (15-24)",
    grepl("overage2plus", indicator) ~ "Overage (by 2yrs) primary students",
    grepl("preschool_1ybefore", indicator) ~ "Primary school attendance 1 year early",
    grepl("preschool_3", indicator) ~ "Primary school attendance",
    grepl("trans_lowsec", indicator) ~ "Transition rate to upper secondary school", 
    grepl("trans_prim", indicator) ~ "Transition rate to lower secondary school")) %>% 
  rename(sex = Sex) %>% 
  filter(!is.na(percentage), !percentage < 0.04, 
         !indicator %in% c("eduyears_2024_m", # eduyears is not a % so won't work in our graph
                           "comp_prim_v2_m", "overage2plus_m",  # removes completion of primary (3-5 years above graduation age) and overage primary school
                           "comp_upsec_v2_m", "comp_lowsec_v2_m",  # removes completion of lower and upper secondary (3-5 years above graduation age)
                           "trans_prim_m", "trans_lowsec_m")) %>%  # removes transition rates into lower and upper secondary
  mutate(percentage = percentage*100) 
 
# Creating a wealth dataframe ----

AFG_data_wealth <- AFG_data %>% 
  filter(grepl("Sex", category), 
         !grepl("Location", category),  # removes data associated with location, region, religion, and ethnicity
         !grepl("Region", category), 
         !grepl("Religion", category), 
         !grepl("Ethnicity", category)) %>% 
  select(-Location, -Region, -Ethnicity, -Religion, -Language) %>% 
  pivot_longer(data =., cols = 6:48, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator), 
         !grepl("Male", Sex), 
         indicator == "literacy_1524_m") %>%  # line of code selects for only female youth literacy data 
  rename(sex = Sex, wealth = Wealth) %>%  # youth literacy according to wealth
  mutate(percentage = percentage*100)

# Creating an ethnicity dataframe ----

AFG_data_ethnicity <- AFG_data %>% 
  filter(grepl("Sex", category), 
         !grepl("Location", category),
         !grepl("Region", category), 
         !grepl("Religion", category), 
         !grepl("Wealth", category)) %>% 
  select(-Location, -Region, -Wealth, -Religion, -Language) %>% 
  pivot_longer(data =., cols = 6:48, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator), 
         !grepl("Male", Sex), 
         indicator == "literacy_1524_m") %>% 
  rename(sex = Sex, ethnicity = Ethnicity) %>%  # youth literacy according to ethnicity
  mutate(percentage = percentage*100)


#Helene's plot = creating the male vs. female education indicators plot ----

sex_palette <- c("#A8DADC", "#1D3557")  # creates colour palette 
names(sex_palette) <- levels(AFG_data_sex$sex)  # assigns colours to sex factor levels

(main_plot <- ggplot(data = AFG_data_sex, aes(x = indicator_name, y = percentage, fill = sex)) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge(), colour = "black") +
    coord_flip() + 
    scale_fill_manual(values = sex_palette) +
    labs(x = "", 
         y = "Percentage", 
         title = "Afghanistan: Gender Inequalities in Education", 
         fill = "Sex") +
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +  # removes empty space between bars and axes
    theme_bw()+
    david_style() +
    theme(legend.position = "right"))

ggsave(main_plot, file = "img/all_indicators_ver2.png", width = 8, height = 8)

# Ana's work = creating the wealth vs. female barplot on youth literacy rates ----

# Creating colour palette for plot
wealth_palette <- c("#E63946", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC")  # assigns colours to wealth_palette
names(wealth_palette) <- levels(AFG_data_wealth$wealth)  # assigns colors to wealth factor levels in AFG_data_wealth data frame

# Plotting wealth x female literacy
(AFG_wealth_bar <- AFG_data_wealth %>% 
    ggplot(aes(x = reorder(wealth, desc(percentage)), y = percentage)) +  # reorders wealth quintiles from richest (Quintile 5) to poorest (Quintile 1)
    geom_bar(stat = "identity", aes(fill = wealth), color = "black") + 
    labs(x = "\nWealth", 
         y = "Youth Literacy Rate (%)\n", 
         title = "Influence of Wealth on Female Youth Literacy Rates") +
    scale_fill_manual(values = wealth_palette) +  # uses custom colour palette
    scale_x_discrete(labels = c("Quintile 5", "Overall", "Quintile 4", "Quintile 3", "Quintile 1", "Quintile 2")) +  # renames bars 
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +  # gets rid of empty space between the x axis and bars 
    theme_bw() + 
    david_style() + 
    theme(axis.text = element_text(angle = 45, hjust = 1)))

ggsave(AFG_wealth_bar, file = "img/female_wealth_youth_lit.png", width = 8, height = 5)

# Ailsa's Plot= ethnicity vs. female youth literacy rates ----

# Adding 'Total Female' to Ethnicity dataset
AFG_data_ethnicity$ethnicity <- as.character(AFG_data_ethnicity$ethnicity)
AFG_data_ethnicity[1,5] <- "Overall"

# Creating colour palette for plot 
ethnicity_palette <- c("#E63946", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC")  # assigns colours to ethnicity palette 
names(ethnicity_palette) <- levels(AFG_data_ethnicity$ethnicity)  # assigns colors to ethnicity factor levels in AFG_data_ethnicity data frame

# Plotting ethnicity x female literacy
(AFG_ethnicity_bar<-AFG_data_ethnicity %>% 
    ggplot(aes(x=reorder(ethnicity, desc(percentage)), y=percentage, fill=category)) +
    geom_bar(stat="identity", colour = "black") +
    labs(x="\nEthnicity", 
         y="Youth Literacy Rate (%)\n", 
         title="Influence of Ethnicity on Female Youth Literacy Rates") +
    scale_fill_manual(values = ethnicity_palette) +  # uses custom colours 
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +  # gets rid of empty space between the x axis and bars 
    theme_bw() +
    david_style() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(AFG_ethnicity_bar, file = "img/female_ethnicity_youth_lit.png", height = 5, width = 8)

# Arranging all plots into panels ----
#Barbora makes final panel
#install.packages("cowplot")

a <- plot_grid(AFG_wealth_bar, AFG_ethnicity_bar, ncol = 2)

(panel <- plot_grid(main_plot, a, nrow = 2))
ggsave2("img/final_panel_ver2.png", width = 12, height = 7)
