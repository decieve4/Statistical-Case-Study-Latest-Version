data = read.csv(file = "./SEM2/Project1/Data/CPepIns.csv")

set.seed(42)

# rebuild data
data$C.Peptide <- as.numeric(data$C.Peptide)
# data$C.Peptide[is.na(data$C.Peptide)] <- sample(1:4, 
#                                                 sum(is.na(data$C.Peptide)), replace = TRUE)
# 
data$Insulin <- as.numeric(data$Insulin)
# data$Insulin[is.na(data$Insulin)] <- 1e-20

data <- data[!is.na(data$C.Peptide) & !is.na(data$Insulin), ]

child.F.Insulin <- 4657
child.L.Insulin <- 1099

child.F.C.Peptide <- 169
child.L.C.Peptide <- 264

children <- data.frame(
  C.Peptide = c(child.F.C.Peptide, child.L.C.Peptide),
  Insulin = c(child.F.Insulin, child.L.Insulin),
  Year = c("Child F", "Child L")
)

# do visualization
library(ggplot2)

ggplot(data = data, aes(x = Insulin, y = C.Peptide, color = factor(Year))) +
  geom_point(size = 3, alpha = 0.3) +
  geom_point(
    data = children, 
    aes(x = Insulin, y = C.Peptide), 
    size = 5, shape = 18
    ) +
  # geom_text(
  #   data = children, 
  #   aes(x = Insulin, y = C.Peptide, label = Year), 
  #   color = "black", 
  #   size = 5, 
  #   vjust = -1
  # ) +
  # scale_color_brewer(palette = "Set1") +
  # scale_x_log10() +
  # scale_y_log10() +
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

# clean data
library(dplyr)

# func_cleandata <- function(data, mode = "NULL"){
#   
#   logcol1 <- log(data$C.Peptide)
#   logcol2 <- log(data$Insulin)
#   
#   if (mode == "log") {
# 
#     lb1_log <- mean(logcol1) - 3 * sd(logcol1)
#     ub1_log <- mean(logcol1) + 3 * sd(logcol1)
#     
#     lb2_log <- mean(logcol2) - 3 * sd(logcol2)
#     ub2_log <- mean(logcol2) + 3 * sd(logcol2)
#     
#     alarm(logcol1)
#     alarm(lb1_log)
#     
#     clean_data <- data %>%
#       filter(logcol1 >= lb1_log & logcol1 <= ub1_log) %>%
#       filter(logcol2 >= lb2_log & logcol2 <= ub2_log)
#     
#   } else if (mode == "IQR") {
# 
#     IQR1 <- IQR(data$C.Peptide)
#     Q1_1 <- quantile(data$C.Peptide, 0.25)
#     Q3_1 <- quantile(data$C.Peptide, 0.75)
#     lb1 <- Q1_1 - 1.5 * IQR1
#     ub1 <- Q3_1 + 1.5 * IQR1
#     
#     IQR2 <- IQR(data$Insulin)
#     Q1_2 <- quantile(data$Insulin, 0.25)
#     Q3_2 <- quantile(data$Insulin, 0.75)
#     lb2 <- Q1_2 - 1.5 * IQR2
#     ub2 <- Q3_2 + 1.5 * IQR2
#     
#     clean_data <- data %>%
#       filter(C.Peptide >= lb1 & C.Peptide <= ub1) %>%
#       filter(Insulin >= lb2 & Insulin <= ub2)
#     
#   } else {
#     
#     stop("Not Implement. Please choose either 'log' or 'IQR'.")
#     
#   }
#   
#   return(clean_data)
# }

# clean_data <- func_cleandata(data, mode = "IQR")
clean_data <- data

ggplot(data = clean_data, aes(x = log(Insulin), y = log(C.Peptide), color = factor(Year))) +
  geom_point(size = 3, alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +
  geom_point(
    data = children, 
    aes(x = log(Insulin), y = log(C.Peptide)), 
    size = 5, shape = 18
  ) +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    title = "Log-Log Scatter Plot of C.Peptide and Insulin by Year",
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

# 3. judge the childrens

test_children <- data.frame(
  Insulin = c(child.F.Insulin, child.L.Insulin)
)

model = lm(data = clean_data, log(C.Peptide) ~ log(Insulin))
summary(model)

# plot(log(C.Peptide) ~ log(Insulin), data = clean_data, main = "Log-Log Regression Line",
#      xlab = "log(Insulin)", ylab = "log(C.Peptide)", pch = 19, col = "blue")
# 
# abline(model)

par(mfrow = c(2, 2))
plot(model)

cbind(
  C.Peptide = c(child.F.C.Peptide, child.L.C.Peptide),
  test_children,
  C.Peptide.pred = exp(coef(model)[1] + log(test_children$Insulin) * coef(model)[2])
)

cbind(
  C.Peptide = log(c(child.F.C.Peptide, child.L.C.Peptide)),
  log(test_children),
  C.Peptide.pred = coef(model)[1] + log(test_children$Insulin) * coef(model)[2]
)

# 4. find best model
# gamma_model <- glm(log(C.Peptide) ~ log(Insulin), data = clean_data, family = Gamma(link = "log"))
# summary(gamma_model)
# 
# par(mfrow = c(2, 2))
# plot(gamma_model)
# 
# cbind(
#   C.Peptide = c(child.F., child.L.),
#   test_children, 
#   C.Peptide.pred = exp(predict(gamma_model, log(test_children)))
# )
# 
# cbind(
#   C.Peptide =log(c(child.F., child.L.)),
#   log(test_children), 
#   C.Peptide.pred = predict(gamma_model, log(test_children))
# )

ggplot(data = clean_data, aes(x = log(Insulin), y = log(C.Peptide), color = factor(Year))) +
  geom_point(size = 3, alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, color = "black") +
  geom_point(
    data = children, 
    aes(x = log(Insulin), y = log(C.Peptide)), 
    size = 5, shape = 18
  ) +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    title = "Log-Log Scatter Plot of C.Peptide and Insulin by Year",
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

# 3. judge the childrens

model = lm(data = clean_data, log(C.Peptide) ~ log(Insulin) + I(log(Insulin)^2))
summary(model)

# plot(log(C.Peptide) ~ log(Insulin), data = clean_data, main = "Log-Log Regression Line",
#      xlab = "log(Insulin)", ylab = "log(C.Peptide)", pch = 19, col = "blue")
# 
# abline(model)

par(mfrow = c(2, 2))
plot(model)

cbind(
  C.Peptide = c(child.F.C.Peptide, child.L.C.Peptide),
  test_children,
  C.Peptide.pred = exp(coef(model)[1] + log(test_children$Insulin) * coef(model)[2] + log(test_children$Insulin) ^ 2 * coef(model)[3])
)

cbind(
  C.Peptide = log(c(child.F.C.Peptide, child.L.C.Peptide)),
  log(test_children),
  C.Peptide.pred = coef(model)[1] + log(test_children$Insulin) * coef(model)[2] + log(test_children$Insulin) ^ 2 * coef(model)[3]
)
