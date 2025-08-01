data = read.csv("./SEM2/Project2/Data/SCS_demand_modelling.csv")
data = data[, -1] # drop empty col
data$wdayindex <- as.factor(data$wdayindex)
data$monthindex <- as.factor(data$monthindex)
data$start_year <- as.factor(data$start_year)
data$DSN <- as.factor(data$DSN)

data_temp = read.csv("./SEM2/Project2/Data/SCS_hourly_temp.csv")