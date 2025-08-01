source("SEM2/Project2/LoadData.R")
full_model <- lm(demand_gross ~ wind + solar_S + TE + wdayindex + start_year + DSN, data = data)
reduced_model <- lm(demand_gross ~ wind + TE + wdayindex + start_year + DSN, data = data)

summary_full <- summary(full_model)
summary_reduced <- summary(reduced_model)

AIC_full <- AIC(full_model)
AIC_reduced <- AIC(reduced_model)

BIC_full <- BIC(full_model)
BIC_reduced <- BIC(reduced_model)

R2_full <- summary_full$r.squared
R2_reduced <- summary_reduced$r.squared

solarS_coef <- summary_full$coefficients["solar_S", 1]
solarS_pval <- summary_full$coefficients["solar_S", 4]

comparison <- data.frame(
  Model = c("Full Model (with solar_S)", "Reduced Model (without solar_S)"),
  R2 = c(R2_full, R2_reduced),
  AIC = c(AIC_full, AIC_reduced),
  BIC = c(BIC_full, BIC_reduced)
)

print(comparison)

solar_S_significance <- data.frame(
  Coefficient = solarS_coef,
  P_Value = solarS_pval
)

print("Solar_S Variable Significance:")
print(solar_S_significance)

par(mfrow = c(2, 2))
plot(full_model)
plot(reduced_model)