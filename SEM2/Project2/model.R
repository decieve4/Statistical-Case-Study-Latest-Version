source("SEM2/Project2/LoadData.R")

# baseline model with all predictor
model <- lm(
  demand_gross ~ wind + TE + wdayindex + monthindex + start_year + DSN,
  data = data
)

# model check
summary(model)

# scatter of response and fitted value with wrong hat of sigma2
fitted_values <- predict(model, newdata = data)
plot(data$demand_gross, fitted_values, 
     main = "Original vs Fitted Data", 
     xlab = "Original Data (demand_gross)", 
     ylab = "Fitted Data (Predicted)", 
     pch = 16, col = "blue")

# plot response and fitted value in same axis
plot(data$demand_gross, type = "l", col = "blue", 
     xlab = "Date", ylab = "Demand Gross", main = "Original and Fitted Demand")
lines(fitted_values, col = "red", lwd = 2)

# residual analysis
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# step to see what happens
step(model)

final_model <- lm(
  demand_gross ~ wind + TE + wdayindex + start_year + DSN,
  data = data
)

summary(final_model)
