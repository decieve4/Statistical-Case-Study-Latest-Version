data = read.csv(file = "./SEM2/Project1/Data/CPepIns.csv")

data$C.Peptide <- as.numeric(data$C.Peptide)
data$C.Peptide[is.na(data$C.Peptide)] <- sample(0:4, 
                                                sum(is.na(data$C.Peptide)), replace = TRUE)

data$Insulin <- as.numeric(data$Insulin)
data$Insulin[is.na(data$Insulin)] <- 0

library(ggplot2)

ggplot(data = data, aes(x = Insulin, y = C.Peptide, color = factor(Year))) +
  geom_point(size = 3, alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) - 1, se = TRUE, color = "black") +
  labs(
    title = "Scatter Plot of C.Peptide and Insulin by Year",
    x = "Insulin",
    y = "C.Peptide",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

model = lm(data = data, C.Peptide ~ Insulin + I(Insulin^2) - 1)
summary(model)

library(dplyr)

data_2023 <- data %>% filter(Year == 2023)

ggplot(data = data_2023, aes(x = Insulin, y = C.Peptide)) +
  geom_point(size = 3, alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) - 1, se = TRUE, color = "black") +
  labs(
    title = "Scatter Plot of C.Peptide and Insulin (2023 Data Only)",
    x = "Insulin",
    y = "C.Peptide"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
