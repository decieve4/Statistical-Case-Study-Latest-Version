source("SEM2/Project2/LoadData.R")
source("SEM2/Project2/Temp Data Set.R")

model_TE <- lm(demand_gross ~ start_year + wdayindex  + wind + TE  + DSN, data = data_final)
model_TO <- lm(demand_gross ~ start_year + wdayindex  + wind + TO  + DSN, data = data_final)
model_temp <- lm(demand_gross ~ start_year + wdayindex  + wind + temp  + DSN, data = data_final)
model_TA <- lm(demand_gross ~ start_year + wdayindex  + wind + TA  + DSN, data = data_final)
model_TMAX <- lm(demand_gross ~ start_year + wdayindex + wind + TMAX  + DSN, data = data_final)
model_TMIN <- lm(demand_gross ~ start_year + wdayindex + wind + TMIN  + DSN, data = data_final)
model_T5 <- lm(demand_gross ~ start_year + wdayindex + wind + T5  + DSN, data = data_final)
model_T10 <- lm(demand_gross ~ start_year + wdayindex + wind + T10  + DSN, data = data_final)
model_TEA <- lm(demand_gross ~ start_year + wdayindex + wind + TEA  + DSN, data = data_final)
model_TEMIN <- lm(demand_gross ~ start_year + wdayindex + wind + TEMIN  + DSN, data = data_final)
model_TEMAX <- lm(demand_gross ~ start_year + wdayindex + wind + TEMAX  + DSN, data = data_final)
model_TE5 <- lm(demand_gross ~ start_year + wdayindex + wind + TE5  + DSN, data = data_final)
model_TE10 <- lm(demand_gross ~ start_year + wdayindex  + wind + TE10 + DSN, data = data_final)
summary(model_TE10)
summary(model_TA)

summary_TE<-summary(model_TE)
summary_TO<-summary(model_TO)
summary_temp<-summary(model_temp)
summary_TA<-summary(model_TA)
summary_TMAX<-summary(model_TMAX)
summary_TMIN<-summary(model_TMIN)
summary_T5<-summary(model_T5)
summary_T10<-summary(model_T10)
summary_TEA<-summary(model_TEA)
summary_TEMIN<-summary(model_TEMIN)
summary_TEMAX<-summary(model_TEMAX)
summary_TE5<-summary(model_TE5)
summary_TE10<-summary(model_TE10)
summary(model_TE10)
# **提取关键指标**
AIC_TE <- AIC(model_TE)
AIC_TO <- AIC(model_TO)
AIC_temp <- AIC(model_temp)
AIC_TA <- AIC(model_TA)
AIC_TMAX <- AIC(model_TMAX)
AIC_TMIN <- AIC(model_TMIN)
AIC_T5 <- AIC(model_T5)
AIC_T10 <- AIC(model_T10)
AIC_TEA <- AIC(model_TEA)
AIC_TEMAX <- AIC(model_TEMAX)
AIC_TEMIN <- AIC(model_TEMIN)
AIC_TE5 <- AIC(model_TE5)
AIC_TE10 <- AIC(model_TE10)


BIC_TE <- BIC(model_TE)
BIC_TO <- BIC(model_TO)
BIC_temp <- BIC(model_temp)
BIC_TA <- BIC(model_TA)
BIC_TMAX <- BIC(model_TMAX)
BIC_TMIN <- BIC(model_TMIN)
BIC_T5 <- BIC(model_T5)
BIC_T10 <- BIC(model_T10)
BIC_TEA <- BIC(model_TEA)
BIC_TEMAX <- BIC(model_TEMAX)
BIC_TEMIN <- BIC(model_TEMIN)
BIC_TE5 <- BIC(model_TE5)
BIC_TE10 <- BIC(model_TE10)

R2_TE <- summary_TE$r.squared
R2_TO <- summary_TO$r.squared
R2_temp <- summary_temp$r.squared
R2_tA <- summary_TA$r.squared
R2_tMAX <- summary_TMAX$r.squared
R2_tMIN <- summary_TMIN$r.squared
R2_t5 <- summary_T5$r.squared
R2_t10 <- summary_T10$r.squared
R2_tEA <- summary_TEA$r.squared
R2_tEMAX <- summary_TEMAX$r.squared
R2_tEMIN <- summary_TEMIN$r.squared
R2_tE5 <- summary_TE5$r.squared
R2_tE10 <- summary_TE10$r.squared


# **提取 TE, TO, temp 变量的系数和 P 值**
TE_coef <- summary_TE$coefficients["TE", 1]
TE_pval <- summary_TE$coefficients["TE", 4]

TO_coef <- summary_TO$coefficients["TO", 1]
TO_pval <- summary_TO$coefficients["TO", 4]

temp_coef <- summary_temp$coefficients["temp", 1]
temp_pval <- summary_temp$coefficients["temp", 4]

tA_coef <- summary_TA$coefficients["TA", 1]
tA_pval <- summary_TA$coefficients["TA", 4]

tMAX_coef <- summary_TMAX$coefficients["TMAX", 1]
tMAX_pval <- summary_TMAX$coefficients["TMAX", 4]

tMIN_coef <- summary_TMIN$coefficients["TMIN", 1]
tMIN_pval <- summary_TMIN$coefficients["TMIN", 4]

t5_coef <- summary_T5$coefficients["T5", 1]
t5_pval <- summary_T5$coefficients["T5", 4]

t10_coef <- summary_T10$coefficients["T10", 1]
t10_pval <- summary_T10$coefficients["T10", 4]

tEA_coef <- summary_TEA$coefficients["TEA", 1]
tEA_pval <- summary_TEA$coefficients["TEA", 4]

tEMAX_coef <- summary_TEMAX$coefficients["TEMAX", 1]
tEMAX_pval <- summary_TEMAX$coefficients["TEMAX", 4]

tEMIN_coef <- summary_TEMIN$coefficients["TEMIN", 1]
tEMIN_pval <- summary_TEMIN$coefficients["TEMIN", 4]

tE5_coef <- summary_TE5$coefficients["TE5", 1]
tE5_pval <- summary_TE5$coefficients["TE5", 4]

tE10_coef <- summary_TE10$coefficients["TE10", 1]
tE10_pval <- summary_TE10$coefficients["TE10", 4]
# **创建对比表**
comparison <- data.frame(
  Model = c("TE Model", "TO Model", "Temp Model","TA Model","TMAX Model","TMIN Model","T5 Model","T10 Model","TEA Model","TEMAX Model","TEMIN Model","TE5 Model","TE10 Model"),
  R2 = c(R2_TE, R2_TO, R2_temp,R2_tA,R2_tMAX,R2_tMIN,R2_t5,R2_t10,R2_tEA,R2_tEMAX,R2_tEMIN,R2_tE5,R2_tE10),
  AIC = c(AIC_TE, AIC_TO, AIC_temp,AIC_TA,AIC_TMAX,AIC_TMIN,AIC_T5,AIC_T10,AIC_TEA,AIC_TEMAX,AIC_TEMIN,AIC_TE5,AIC_TE10),
  BIC = c(BIC_TE, BIC_TO, BIC_temp,BIC_TA,BIC_TMAX,BIC_TMIN,BIC_T5,BIC_T10,BIC_TEA,BIC_TEMAX,BIC_TEMIN,BIC_TE5,BIC_TE10),
  Coefficient = c(TE_coef, TO_coef, temp_coef,tA_coef,tMAX_coef,tMIN_coef,t5_coef,t10_coef,tEA_coef,tEMAX_coef,tEMIN_coef,tE5_coef,tE10_coef),
  P_Value = c(TE_pval, TO_pval, temp_pval,tA_pval,tMAX_pval,tMIN_pval,t5_pval,t10_pval,tEA_pval,tEMAX_pval,tEMIN_pval,tE5_pval,tE10_pval)
)
print(comparison)


par(mfrow = c(2, 2))
plot(model_TE10)
par(mfrow = c(1, 1))

data_final$predicted_demand <- predict(model_TE10, newdata = data_final)
mse <- mean((data_final$demand_gross - data_final$predicted_demand)^2)
rmse <- sqrt(mean((data_final$demand_gross - data_final$predicted_demand)^2))
library(ggplot2)
ggplot(data_final, aes(x = demand_gross, y = predicted_demand)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue", linewidth = 1) +
  labs(x = "Actual", y = "Predicted") +
  theme_minimal()
ggplot(data_final, aes(x = demand_gross, y = predicted_demand)) +
  geom_point(color = "#1f77b4", size = 2, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    x = "Actual Demand",
    y = "Predicted Demand"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

