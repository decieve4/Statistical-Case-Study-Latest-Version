library(ggplot2)
library(dplyr)

hourly_temp <- read.csv("./SEM2/Project2/Data/SCS_hourly_temp.csv")

hourly_temp$Date <- as.POSIXct(hourly_temp$Date, format="%d/%m/%Y %H:%M")
hourly_temp$Day <- as.Date(hourly_temp$Date)
hourly_temp$Hour <- as.numeric(format(hourly_temp$Date, "%H"))
hourly_temp <- hourly_temp %>% filter(!is.na(temp) & !is.na(Hour))
unique_days <- unique(hourly_temp$Day)
random_days <- sample(unique_days, min(5, length(unique_days)), replace = FALSE)
subset_data <- hourly_temp %>% filter(Day %in% random_days)
ggplot(subset_data, aes(x = Hour, y = temp, color = as.factor(Day), group = Day)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +  
  labs(
    x = "Hour of the Day",
    y = "Temperature (°C)",
    color = "Date"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
