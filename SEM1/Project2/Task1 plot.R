library(ggplot2)
library(tidyr)
# 创建数据框
data1 <- data.frame(
  topic = c("Baseline"),                     # 主题列
  DA_accuracy = c(0.9669),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.658),               # KNN 精度
  RF_accuracy = c(0.9995)                 # Random Forest 精度
)
write.csv(data1, "output.csv", row.names = FALSE)
# 创建数据框
data2 <- data.frame(
  topic = c(50),                     # 主题列
  DA_accuracy = c(0.8216),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.5211),               # KNN 精度
  RF_accuracy = c(0.4999)                 # Random Forest 精度
)
write.csv(data2, "output.csv", row.names = FALSE)

# 创建数据框
data3 <- data.frame(
  topic = c(200),                     # 主题列
  DA_accuracy = c(0.9261),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.5823),               # KNN 精度
  RF_accuracy = c(0.5084)                 # Random Forest 精度
)
write.csv(data3, "output.csv", row.names = FALSE)

# 创建数据框
data4 <- data.frame(
  topic = c(500),                     # 主题列
  DA_accuracy = c(0.957),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.6),               # KNN 精度
  RF_accuracy = c(0.702)                 # Random Forest 精度
)
write.csv(data4, "output.csv", row.names = FALSE)

# 创建数据框
data5 <- data.frame(
  topic = c(1000),                     # 主题列
  DA_accuracy = c(0.9646),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.6012),               # KNN 精度
  RF_accuracy = c(0.8937)                 # Random Forest 精度
)
write.csv(data5, "output.csv", row.names = FALSE)
# 创建数据框
data6 <- data.frame(
  topic = c(2000),                     # 主题列
  DA_accuracy = c(0.9659),                # Discriminant Analysis 精度
  KNN_accuracy = c(0.6366),               # KNN 精度
  RF_accuracy = c(0.9834)                 # Random Forest 精度
)
write.csv(data6, "output.csv", row.names = FALSE)
Final_Result <- rbind(data1,data2,data3,data4,data5,data6)

colnames(Final_Result) <- c("Dataset", "DA", "KNN", "RF")

Final_Result$Dataset <- factor(Final_Result$Dataset, levels = c("Baseline", "50", "200", "500", "1000", "2000"))

Result_long <- pivot_longer(Final_Result, 
                            cols = colnames(Final_Result)[-1], 
                            names_to = "Model", 
                            values_to = "Accuracy") 

Baseline_long <- Result_long[1:3, ]
Result_long <- Result_long[-c(1, 2, 3), ]

fig <- ggplot(Result_long, aes(x = Dataset, y = Accuracy, color = Model, group = Model)) +
  geom_line(linewidth = 1.5) +                # Plot the lines connecting the points for each model
  geom_point(size = 4) +               # Add points to show the accuracy values
  labs(x = "Number of Words",  # X-axis label
       y = "Accuracy") +  # Y-axis label
  scale_color_brewer(palette = "Set2") + 
  geom_hline(data = Baseline_long, 
             aes(yintercept = Accuracy, color = Model), 
             linetype = "dashed", linewidth = 1) +# Add horizontal dashed lines for Baseline
  theme_bw() + 
  theme(
    text = element_text(size = 14),  # Set global font size
    axis.title = element_text(size = 16),  # Adjust axis title font size
    axis.text = element_text(size = 12),  # Adjust axis tick labels font size
    legend.title = element_text(size = 14),  # Adjust legend title font size
    legend.text = element_text(size = 12),  # Adjust legend text font size
    plot.title = element_text(size = 18)  # Adjust plot title font size
  )


ggsave("./SEM1/Project2/Figures/Task1 ReduceWords Lineplot.pdf", width = 8, height = 6)
