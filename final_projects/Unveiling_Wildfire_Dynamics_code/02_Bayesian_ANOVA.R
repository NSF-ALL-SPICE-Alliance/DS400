# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

# data 
dat <- read_csv("./datasets/anova_data.csv")

CA_hierarchical <- stan_glmer(log_fire_dens ~ (1 | county),
                              data = dat,
                              family = gaussian,
                              chains = 4,
                              iter = 5000 * 2,
                              seed = 84735)
  
# Confirm the prior tunings
prior_summary(CA_hierarchical)

pp_check(CA_hierarchical, nreps = 50) +
  scale_colour_manual(values = c("#440154FF", "#ffcf20FF")) +
  xlab("log_fire_dens") +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 16),  # Adjust x-axis label size
    axis.text.y = element_text(size = 16),  # Adjust y-axis label size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.text = element_text(size = 16),  # Adjust legend text size
    legend.title = element_text(size = 16)   # Adjust legend title size
  ) 

# Store the simulation in a data frame
CA_hierarchical_df <- as.data.frame(CA_hierarchical)

# Check out the first 3 and last 3 parameter labels
CA_hierarchical_df %>% 
  colnames() %>% 
  as.data.frame() %>% 
  slice(1:3, 45:47)


tidy(CA_hierarchical, effects = "fixed", 
     conf.int = TRUE, conf.level = 0.80)
tidy(CA_hierarchical, effects = "ran_pars")

county_summary <- tidy(CA_hierarchical, effects = "ran_vals", 
                       conf.int = TRUE, conf.level = 0.80)

# Check out the results for the first & last 2 countys
county_summary %>% 
  select(level, conf.low, conf.high) %>% 
  slice(1:2, 43:44)


# Get MCMC chains for each mu_j
county_chains <- CA_hierarchical %>%
  spread_draws(`(Intercept)`, b[,county]) %>% 
  mutate(mu_j = `(Intercept)` + b) 

# Check it out
county_chains %>% 
  select(county, `(Intercept)`, b, mu_j) %>% 
  head(4)


# Get posterior summaries for mu_j
county_summary_scaled <- county_chains %>% 
  select(-`(Intercept)`, -b) %>% 
  mean_qi(.width = 0.80) %>% 
  mutate(county = fct_reorder(county, mu_j))

# Check out the results
county_summary_scaled %>% 
  select(county, mu_j, .lower, .upper) %>% 
  head(4)


ggplot(county_summary_scaled, 
       aes(x = county, y = mu_j, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  xaxis_text(angle = 90, hjust = 1)



# Remove "county:" from the beginning of the county variable
county_summary_scaled <- county_summary_scaled %>%
  mutate(county = gsub("^county:", "", county))

# Print the updated data frame
print(county_summary_scaled)


county_summary_scaled %>% 
  ggplot(aes(x = county, y = mu_j, ymin = .lower, ymax = .upper, size = 2)) +
  geom_pointrange(col = "#440154FF", size = 1.2, lwd = 1.2) +
  theme_minimal() +
  xaxis_text(angle = 45, hjust = 1) +
  labs(x = "",
       y = "Log fire density") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))  # Make y-axis label bold and size 15



county_summary_scaled %>% 
  mutate(county = fct_reorder(county, mu_j)) %>% 
  ggplot(aes(x = county, y = mu_j, ymin = .lower, ymax = .upper)) +
  geom_pointrange(col = "#440154FF") +
  theme_minimal() +
  xaxis_text(angle = 45, hjust = 1) +
  labs(x = "",
       y = "Log fire density") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 15))  # Adjust the size as needed

