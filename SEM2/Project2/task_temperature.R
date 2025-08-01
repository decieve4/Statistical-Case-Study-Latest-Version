source("SEM2/Project2/LoadData.R")

# filter the same day's temperature data
data_temp$Date <- as.POSIXct(data_temp$Date, format = "%d/%m/%Y %H:%M")
data_temp$day <- format(data_temp$Date, "%Y-%m-%d")
data$Date <- as.Date(data$Date)
data_temp$day <- as.Date(data_temp$day)
data_temp_filtered <- data_temp[data_temp$day %in% data$Date, ]

library(dplyr)
df_daily <- data_temp_filtered %>%
  mutate(hour = as.numeric(format(Date, "%H"))) %>%
  group_by(day) %>%
  summarise(
    T_AVG_10_15 = mean(temp[hour %in% 10:15], na.rm = TRUE),
    T_AVG_5_20 = mean(temp[hour >= 5 & hour <= 20], na.rm = TRUE),
    T_MAX = max(temp, na.rm = TRUE),
    T_MIN = min(temp, na.rm = TRUE),
    T_AVG   = mean(temp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(day)

df_daily$TE_AVG_10_15  <- NA_real_
df_daily$TE_AVG_5_20   <- NA_real_
df_daily$TE_MAX <- NA_real_
df_daily$TE_MIN <- NA_real_
df_daily$TE_AVG   <- NA_real_
init_val <- 11.3652008

df_daily$TE_AVG_10_15[1]  <- init_val
df_daily$TE_AVG_5_20[1]   <- init_val
df_daily$TE_MAX[1] <- init_val
df_daily$TE_MIN[1] <- init_val
df_daily$TE_AVG[1]   <- init_val

for(i in 2:nrow(df_daily)){
  df_daily$TE_AVG_10_15[i]  <- (df_daily$T_AVG_10_15[i]  + df_daily$TE_AVG_10_15[i-1])  / 2
  df_daily$TE_AVG_5_20[i]   <- (df_daily$T_AVG_5_20[i]   + df_daily$TE_AVG_5_20[i-1])   / 2
  df_daily$TE_MAX[i] <- (df_daily$T_MAX[i] + df_daily$TE_MAX[i-1]) / 2
  df_daily$TE_MIN[i] <- (df_daily$T_MIN[i] + df_daily$TE_MIN[i-1]) / 2
  df_daily$TE_AVG[i]   <- (df_daily$T_AVG[i]   + df_daily$TE_AVG[i-1])   / 2
}

data <- data %>%
  left_join(df_daily, by = c("Date" = "day"))

rm(df_daily, data_temp_filtered)
rm(init_val)

final_model <- lm(
  demand_gross ~ wind + TE_AVG_10_15 + wdayindex + start_year + DSN,
  data = data
)
summary(final_model)
