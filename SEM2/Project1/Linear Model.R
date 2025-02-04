data = read.csv(file = "./SEM2/Project1/Data/CPepIns.csv")

length(data)

data$C.Peptide <- as.numeric(data$C.Peptide)
data$C.Peptide[is.na(data$C.Peptide)] <- sample(0:4, 
                                                sum(is.na(data$C.Peptide)), replace = TRUE)
mean_Cvalue <- mean(data$C.Peptide)
summary(data$C.Peptide)
# Step 2: Calculate the standard error (SE)
sd_Cvalue <- sd(data$C.Peptide)


# Step 3: Define the thresholds
lower_Cbound <- mean_Cvalue - 3 * sd_Cvalue
upper_Cbound <- mean_Cvalue + 3 * sd_Cvalue
data$Insulin <- as.numeric(data$Insulin)
data$Insulin[is.na(data$Insulin)] <- 0
mean_Ivalue <- mean(data$Insulin)

# Step 2: Calculate the standard error (SE)
sd_Ivalue <- sd(data$Insulin)


# Step 3: Define the thresholds
lower_Ibound <- mean_Ivalue - 3 * sd_Ivalue
upper_Ibound <- mean_Ivalue + 3 * sd_Ivalue

# Step 4: Filter the data within the range
filtered_data <- data[data$Insulin >= lower_Ibound & data$Insulin <= upper_Ibound & data$C.Peptide >= lower_Cbound & data$C.Peptide <= upper_Cbound, ]


data<-filtered_data

model1<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2),data= data)
summary(model1)
model2<- lm(data$C.Peptide~data$Insulin-1,data= data)
summary(model2)
model3<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3),data= data)
summary(model3)
model4<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4),data= data)
summary(model4)
model5<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5),data= data)
summary(model5)
model6<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5)+I(data$Insulin^6),data= data)
summary(model6)
model7<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5)+I(data$Insulin^6)+I(data$Insulin^7),data= data)
summary(model7)
model8<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5)+I(data$Insulin^6)+I(data$Insulin^7)+I(data$Insulin^8),data= data)
summary(model8)
model9<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5)+I(data$Insulin^6)+I(data$Insulin^7)+I(data$Insulin^8)+I(data$Insulin^9),data= data)
summary(model9)
model10<- lm(data$C.Peptide~data$Insulin+I(data$Insulin^2)+I(data$Insulin^3)+I(data$Insulin^4)+I(data$Insulin^5)+I(data$Insulin^6)+I(data$Insulin^7)+I(data$Insulin^8)+I(data$Insulin^9)+I(data$Insulin^10),data= data)
summary(model10)
anova(model9,model10)
AIC(model4, model10)
par(mfrow=c(2,2))
plot(model4)

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
step(model3)
drop1(model4)
