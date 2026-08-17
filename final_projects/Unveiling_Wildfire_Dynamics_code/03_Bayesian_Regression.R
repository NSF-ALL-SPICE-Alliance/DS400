library(bayesrules)
library(tidycensus)
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(bayesplot)
library(broom.mixed)
library(sf) 


################################################################################
d00 <- read_csv("./datasets/mapdataall.csv")
d01 <- d00 %>%
  group_by(year = year(incident_date_created)) %>%
  summarise(mean = mean(incident_acres_burned, na.rm = TRUE))

library(scales)
ggplot(d01, aes(x = year, y = mean)) +
  geom_line(size = 1, col = "#440154FF") +
  scale_x_continuous(limits = c(2013, 2023), breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(labels = comma) +  # Add commas to y-axis labels
  theme_minimal() +
  labs(
    x = "Year",
    y = "Avg. acres burnt"
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
rm(d00, d01)
# ------------------------------------------------------------------------------


# data 
final_data <- st_read("./datasets/final_data.geojson")
names(final_data)[c(5, 17, 18, 19)] = c("population", "elevation", "temperature", "precipitation") 

# No transformation ############################################################
# density plot for "fire_dens" very Skewed  
p1 <-
  final_data %>% 
  ggplot(aes(x = fire_dens)) +
    geom_density(size = 1, col = "#440154FF") +
    #xlim(c(-.01, 0.4)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      axis.text.x = element_text(size = 12),
      # Adjust x-axis label size
      axis.text.y = element_text(size = 12),
      # Adjust y-axis label size
      axis.title.x = element_text(size = 14),
      # Adjust x-axis title size
      axis.title.y = element_text(size = 14),
      # Adjust y-axis title size
    ) +
    labs(x = "Fire density",
         y = "Density",
         title = "(a)" ) 


# log transformation ###########################################################

# density plot for "lo_fire_dens" very Skewed  
p2 <-
  final_data %>%
    ggplot(aes(x = log_fire_dens)) +
    geom_density(size = 1, col = "#440154FF") +
    #xlim(c(-12, 5)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      axis.text.x = element_text(size = 12),
      # Adjust x-axis label size
      axis.text.y = element_text(size = 12),
      # Adjust y-axis label size
      axis.title.x = element_text(size = 14),
      # Adjust x-axis title size
      axis.title.y = element_text(size = 14),
      # Adjust y-axis title size
    ) +
    labs(x = "Log of fire density",
         y = "",
         title = "(b)" ) 

# Calculate Shapiro-Wilk p-values
shapiro_pvalue_fire_dens <- shapiro.test(final_data$fire_dens)$p.value
shapiro_pvalue_log_fire_dens <- shapiro.test(log(final_data$fire_dens))$p.value

# Create a Q-Q plot for fire_dens using ggplot2
p3 <- ggplot(data = final_data, aes(sample = fire_dens)) +
  geom_qq(color = "#F9C802", size = 2) +
  geom_qq_line(color = "#440154FF", size = 1) +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = expression("Shapiro test p-value = " ~ 6.67 %*% 10^-13),
    hjust = -0.1, vjust = 2.1, size = 4.2, color = "#440154FF"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles",
       title = "(c)")

# Create a Q-Q plot for log(fire_dens) using ggplot2
p4 <- ggplot(data = final_data, aes(sample = log(fire_dens))) +
  geom_qq(color = "#F9C802", size = 2) +
  geom_qq_line(color = "#440154FF", size = 1) +
  geom_text(
    x = -Inf, y = Inf,
    label = expression("Shapiro test p-value = " ~ 0.223),
    hjust = -0.1, vjust = 3.1, size = 4, color = "#440154FF"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(x = "Theoretical quantiles",
       y = "",
       title = "(d)")

# Arrange the Q-Q plots in a single row
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)



################################################################################



# # Boxplots
# dat %>%
#   ggplot(aes(x = county, y = log_fire_dens)) +
#     geom_boxplot() +
#     theme_minimal() +
#     xaxis_text(angle = 45, hjust = 1) +
#     labs(title = "Boxplot of Transformed Fire Density by County",
#          x = "County",
#          y = "Log of Fire Density") +
#     theme(axis.text.x = element_text(size = 15))  # Adjust the size as needed





final_data <- st_read("./datasets/final_data.geojson")
names(final_data)[c(5, 17, 18, 19)] = c("population", "elevation", "temperature", "precipitation") 


# Map
p11 <- final_data %>%
  ggplot(aes(fill = fire_dens)) +
  geom_sf() +
  scale_fill_viridis_c(name = "Fire density") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 12),  # Adjust x-axis label size
    axis.text.y = element_text(size = 12),  # Adjust y-axis label size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.text = element_text(size = 16),  # Adjust legend text size
    legend.title = element_text(size = 16),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  ) + labs(title = "(a)")

p12 <- final_data %>%
  ggplot(aes(fill = log_fire_dens)) +
  geom_sf() +
  scale_fill_viridis_c(name = "Log fire density") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 12),  # Adjust x-axis label size
    axis.text.y = element_text(size = 12),  # Adjust y-axis label size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.text = element_text(size = 16),  # Adjust legend text size
    legend.title = element_text(size = 16),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  ) + labs(title = "(b)")

gridExtra::grid.arrange(p11, p12, nrow = 1)


# ANOVA ########################################################################
# ------------------------------------------------------------------------------
# "Bayesian_ANOVA" file includes the ANOVA code. 



# Regression ###################################################################
b <- c(
  "San Bernardino",
  "Riverside",
  "Humboldt",
  "San Diego",
  "Fresno",
  "Kern",
  "Shasta",
  "San Luis Obispo",
  "Mendocino",
  "Monterey",
  "Siskiyou",
  "Tulare",
  "El Dorado",
  "Santa Clara",
  "Tehama",
  "Butte",
  "Tuolumne",
  "Lassen",
  "Nevada",
  "Inyo",
  "Madera",
  "San Benito",
  "Los Angeles",
  "Del Norte",
  "Imperial",
  "Modoc",
  "Sacramento",
  "Calaveras"
)


t = c(
  "Stanislaus",
  "Lake",
  "Merced",
  "Alameda",
  "Santa Cruz",
  "Santa Barbara",
  "Contra Costa",
  "Marin",
  "Placer",
  "Yuba",
  "Amador",
  "Solano",
  "Ventura",
  "San Joaquin",
  "San Mateo",
  "Colusa",
  "Sonoma",
  "Trinity",
  "Mariposa",
  "Mono",
  "Sutter",
  "Glenn",
  "Sierra",
  "Plumas",
  "Kings",
  "Napa",
  "Yolo",
  "Orange",
  "Alpine"
)

top_counties <- final_data %>% filter(county %in% t) 
bottom_counties <- final_data %>% filter(county %in% b) 


top <-
  stan_glm(log_fire_dens ~ SOX + 
                           PM10 + 
                           population + 
                           elevation + 
                           temperature + 
                           precipitation,
           data = top_counties,
           family = gaussian,
           chains = 4,
           iter = 5000 * 2,
           seed = 84735)
    

bottom <-
  stan_glm(log_fire_dens ~ SOX + 
                           PM10 + 
                           population + 
                           elevation + 
                           temperature + 
                           precipitation,
           data = bottom_counties,
           family = gaussian,
           chains = 4,
           iter = 5000 * 2,
           seed = 84735)
    

col = c("#FDE725FF", "#2A788EFF", "#414487FF", "#440154FF")

# Regression Results
tidy(top, effects = c("fixed", "aux"), conf.int = TRUE, conf.level = 0.80)
neff_ratio(top)
rhat(top)

tidy(bottom, effects = c("fixed", "aux"), conf.int = TRUE, conf.level = 0.80)
neff_ratio(bottom)
rhat(bottom)


# Trace plots
p31 <-
  mcmc_trace(top, "SOX") + labs(title = "Top Counties") + ylim(-0.5, 0.3) + scale_colour_manual(values = col)
p32 <-
  mcmc_trace(bottom, "SOX") + labs(title = "Bottom counties") + ylim(-0.5, 0.3) + scale_colour_manual(values = col)

p33 <-
  mcmc_trace(top, "PM10") + labs(title = "Top Counties") + ylim(-0.12, 0.1) + scale_colour_manual(values = col)
p34 <-
  mcmc_trace(bottom, "PM10") + labs(title = "Bottom counties") + ylim(-0.12, 0.1) + scale_colour_manual(values = col)

p35 <-
  mcmc_trace(top, "population") + labs(title = "") + ylim(-1, 1.5) + scale_colour_manual(values = col)
p36 <-
  mcmc_trace(bottom, "population") + labs(title = "") + ylim(-1, 1.5) + scale_colour_manual(values = col)

p37 <-
  mcmc_trace(top, "elevation") + labs(title = "") + ylim(-0.002, 0.002) + scale_colour_manual(values = col)
p38 <-
  mcmc_trace(bottom, "elevation") + labs(title = "") + ylim(-0.002, 0.002) + scale_colour_manual(values = col)

p39 <-
  mcmc_trace(top, "temperature") + labs(title = "") + ylim(-0.2, 0.2) + scale_colour_manual(values = col)
p40 <-
  mcmc_trace(bottom, "temperature") + labs(title = "") + ylim(-0.2, 0.2) + scale_colour_manual(values = col)

p41 <-
  mcmc_trace(top, "precipitation") + labs(title = "") + ylim(-0.2, 0.2) + scale_colour_manual(values = col)
p42 <-
  mcmc_trace(bottom, "precipitation") + labs(title = "") + ylim(-0.2, 0.2) + scale_colour_manual(values = col)


#pdf("./07_traceplots1.pdf", height = 6, width = 9)
gridExtra::grid.arrange(p33, p34, p37, p38, p41, p42, nrow = 3) 
#dev.off()

#pdf("./07_traceplots2.pdf", height = 6, width = 9)
gridExtra::grid.arrange(p31, p32, p35, p36, p39, p40, nrow = 3) 
#dev.off()



# # legend location changed ####################################################
# library(cowplot)
# 
# # Your existing code for p31 and p32...
# 
# # Combine legends from both plots
# combined_legend <- get_legend(p31 + theme(legend.position = "bottom"))  # Assuming the legend is at the bottom
# 
# # Remove individual legends from each plot
# p31 <- p31 + theme(legend.position = "none")
# p32 <- p32 + theme(legend.position = "none")
# p33 <- p33 + theme(legend.position = "none")
# p34 <- p34 + theme(legend.position = "none")
# p35 <- p35 + theme(legend.position = "none")
# p36 <- p36 + theme(legend.position = "none")
# p37 <- p37 + theme(legend.position = "none")
# p38 <- p38 + theme(legend.position = "none")
# p39 <- p39 + theme(legend.position = "none")
# p40 <- p40 + theme(legend.position = "none")
# p41 <- p41 + theme(legend.position = "none")
# p42 <- p42 + theme(legend.position = "none")
# 
# # Arrange the plots and add the combined legend
# combined_plot <- plot_grid(p33, p34, p37, p38, p41, p42, nrow = 3, rel_heights = c(1, 1), align = 'v')
# final_plot1 <- plot_grid(combined_plot, combined_legend, nrow = 2, rel_heights = c(1, 0.1))
# 
# # Print the final plot 1
# pdf("C:/Users/moham/OneDrive - The University of Colorado Denver/Desktop/07_traceplots1.pdf", height = 6, width = 9)
# final_plot1
# dev.off()
# 
# # Arrange the plots and add the combined legend
# combined_plot <- plot_grid(p31, p32, p35, p36, p39, p40, nrow = 3, rel_heights = c(1, 1), align = 'v')
# final_plot2 <- plot_grid(combined_plot, combined_legend, nrow = 2, rel_heights = c(1, 0.1))
# 
# # Print the final plot 1
# pdf("C:/Users/moham/OneDrive - The University of Colorado Denver/Desktop/07_traceplots2.pdf", height = 6, width = 9)
# final_plot2
# dev.off()

