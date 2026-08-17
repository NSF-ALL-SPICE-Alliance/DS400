final_data <- st_read("./datasets/final_data.geojson")
names(final_data)[c(5, 17, 18, 19)] = c("population", "elevation", "temperature", "precipitation") 

p1 =
final_data %>%
  ggplot(aes(fill = elevation)) +
  geom_sf() +
  scale_fill_viridis_c(name = "Elevation") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  ) 

p2 = 
final_data %>%
  ggplot(aes(fill = temperature)) +
  geom_sf() +
  scale_fill_viridis_c(name = "Temp") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  ) 

p3= 
final_data %>%
  ggplot(aes(fill = precipitation)) +
  geom_sf() +
  scale_fill_viridis_c(name = "precipitation") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  )

p4 = 
final_data %>%
  ggplot(aes(fill = population)) +
  geom_sf() +
  scale_fill_viridis_c(name = "Population") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  )

p5 = 
final_data %>%
  ggplot(aes(fill = PM10)) +
  geom_sf() +
  scale_fill_viridis_c(name = "PM10") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12),  # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  )

p6 = 
final_data %>%
  ggplot(aes(fill = SOX)) +
  geom_sf() +
  scale_fill_viridis_c(name = "SOX") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 12), # Adjust legend title size
    plot.title = element_text(size = 25, face = "bold")  # Adjust title size to 45 and set font weight to bold
  )

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6,  nrow = 3)
