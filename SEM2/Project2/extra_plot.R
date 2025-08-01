source("SEM2/Project2/task_temperature.R")

par(mfrow = c(2, 1))
par(mfrow = c(2, 1), mar = c(5, 5, 1, 1))
par(mfrow = c(1, 1))
acf(data$demand_gross)
pacf(data$demand_gross)

# Combine data for 2011, 2012, and 2013
weather_condition_2011 <- data[data$start_year == "2011", c("wind", "TE_AVG_10_15", "DSN")]
weather_condition_2011$start_year <- "2011"

weather_condition_2012 <- data[data$start_year == "2012", c("wind", "TE_AVG_10_15", "DSN")]
weather_condition_2012$start_year <- "2012"

weather_condition_2013 <- data[data$start_year == "2013", c("wind", "TE_AVG_10_15", "DSN")]
weather_condition_2013$start_year <- "2013"

# Combine all data into one dataframe
weather_condition_combined <- rbind(weather_condition_2011, weather_condition_2012, weather_condition_2013)

# Create scatter plot using ggplot with enhancements
ggplot(weather_condition_combined, aes(x = wind, y = TE_AVG_10_15, color = start_year)) +
  geom_point(size = 3, alpha = 0.4) +  # Increase point size and make points slightly transparent
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.5) +  # Add trend line with a dashed style
  labs(
    x = "Wind", 
    y = "TE_AVG_10_15"
  ) +
  scale_color_manual(values = c("2011" = "#FF5733", "2012" = "#33C1FF", "2013" = "#2A9D8F")) +  # Custom colors for each year
  theme_minimal(base_size = 15) +  # Increase font size
  theme(
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text = element_text(color = "gray30"),  # Customize axis text color
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Adjust legend text size
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Customize grid lines
    panel.grid.minor = element_line(color = "gray90", size = 0.25)  # Customize minor grid lines
  )


