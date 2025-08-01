source("SEM2/Project2/data_preprocessing.R")

hist(data$demand_gross,
     breaks = 30,
     col = "lightblue",
     border = "white",
     xlab = "18:00 Peak Demand (MW)",
     main = "")