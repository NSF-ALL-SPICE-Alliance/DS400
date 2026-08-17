library(tidyverse)
library(here)
library(rstanarm)
library(readxl)
library(janitor)
library(naniar)
library(randomForest)
library(treeshap)

chr_data <- read_excel(here("data", "2025_County_Health_Rankings_Data.xlsx"), sheet = 2, skip = 1)

chr_data <- chr_data %>%
  clean_names()


chr_data <- chr_data %>%
  select(food_environment_index,
         percent_uninsured,
         percent_severe_housing_problems,
         percent_households_with_broadband_access,
         average_daily_pm2_5,
         percent_unemployed,
         income_ratio,
         percent_children_in_poverty,
         social_association_rate,
         preventable_hospitalization_rate,
         percent_completed_high_school,
         percent_some_college,
         percent_fair_or_poor_health)





ggplot(chr_data, aes(x = percent_fair_or_poor_health)) +
  geom_histogram()


# Challenge - create a map showing percent_fair_or_poor_health by county



naniar::gg_miss_var(chr_data, show_pct = TRUE)

chr_data <- chr_data %>%
  drop_na()


rf = randomForest(percent_fair_or_poor_health ~ ., data = chr_data, ntree = 500)

rf

unified <- unify(rf, chr_data)

treeshap1 <- treeshap(unified,  chr_data[1:200, ], verbose = 0)


plot_feature_importance(treeshap1, max_vars = 6)

plot_feature_dependence(treeshap1, "percent_completed_high_school")

# challenge - make feature dependence plots for the next 4 important variables




plot_contribution(treeshap1, obs = 2)






