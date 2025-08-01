source("SEM2/Project2/LoadData.R")
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
    T10 = mean(temp[hour %in% 10:15], na.rm = TRUE),
    T5  = mean(temp[hour >= 5 & hour <= 20], na.rm = TRUE),
    TMAX = max(temp, na.rm = TRUE),
    TMIN = min(temp, na.rm = TRUE),
    TA   = mean(temp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(day)
df_daily$TE10  <- NA_real_
df_daily$TE5   <- NA_real_
df_daily$TEMAX <- NA_real_
df_daily$TEMIN <- NA_real_
df_daily$TEA   <- NA_real_
init_val <- 11.3652008
df_daily$TE10[1]  <- init_val
df_daily$TE5[1]   <- init_val
df_daily$TEMAX[1] <- init_val
df_daily$TEMIN[1] <- init_val
df_daily$TEA[1]   <- init_val
for(i in 2:nrow(df_daily)){
  df_daily$TE10[i]  <- (df_daily$T10[i]  + df_daily$TE10[i-1])  / 2
  df_daily$TE5[i]   <- (df_daily$T5[i]   + df_daily$TE5[i-1])   / 2
  df_daily$TEMAX[i] <- (df_daily$TMAX[i] + df_daily$TEMAX[i-1]) / 2
  df_daily$TEMIN[i] <- (df_daily$TMIN[i] + df_daily$TEMIN[i-1]) / 2
  df_daily$TEA[i]   <- (df_daily$TA[i]   + df_daily$TEA[i-1])   / 2
}
data_final <- data %>%
  left_join(df_daily, by = c("Date" = "day"))