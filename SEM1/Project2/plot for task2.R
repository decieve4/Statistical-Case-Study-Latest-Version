# Load necessary packages
library(ggplot2)
library(reshape2)

# Load the data from CSV files
current_results <- read.csv("~/Desktop/Stat_Case_Study/SEM1/Project2/Figures/summary_accuracy_results.csv")
baseline_results <- read.csv("~/Desktop/Stat_Case_Study/SEM1/Project2/Figures/baseline_accuracy_results.csv")

# 1. 主题分类准确率对比图（Topic-wise Classification Accuracy）
# Convert current results to long format for ggplot2
current_long <- melt(current_results, id.vars = "topic", variable.name = "model", value.name = "accuracy")

# Plot the topic-wise classification accuracy for Task 2
accuracy_plot <- ggplot(current_long, aes(x = reorder(topic, -accuracy), y = accuracy, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Topic-wise Classification Accuracy for Task 2",
       x = "Topic",
       y = "Accuracy",
       fill = "Model Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(accuracy_plot)

# Save the plot
ggsave("~/Desktop/Stat_Case_Study/SEM1/Project2/Figures/topic_wise_accuracy_task2.png", plot = accuracy_plot, width = 14, height = 8)

# 2. 整体准确率对比图（Overall Classification Accuracy Comparison）
# Calculate overall accuracy for each model in Task 2
overall_current <- data.frame(
  model = c("DA_accuracy", "KNN_accuracy", "RF_accuracy"),
  accuracy = colMeans(current_results[, -1], na.rm = TRUE)
)

# Prepare baseline accuracy
overall_baseline <- data.frame(
  model = c("DA_accuracy", "KNN_accuracy", "RF_accuracy"),
  accuracy = as.numeric(baseline_results[1, 2:4])
)

# Combine baseline and current results for comparison
comparison_overall <- merge(overall_current, overall_baseline, by = "model", suffixes = c("_task2", "_baseline"))

# Convert data to long format for ggplot2
comparison_long <- melt(comparison_overall, id.vars = "model", variable.name = "experiment", value.name = "accuracy")

# Plot the overall classification accuracy comparison
overall_accuracy_plot <- ggplot(comparison_long, aes(x = model, y = accuracy, fill = experiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Overall Classification Accuracy Comparison: Baseline vs Task 2",
       x = "Model Type",
       y = "Accuracy",
       fill = "Experiment")

# Display the plot
print(overall_accuracy_plot)

# Save the plot
ggsave("~/Desktop/Stat_Case_Study/SEM1/Project2/Figures/overall_accuracy_comparison.png", plot = overall_accuracy_plot, width = 10, height = 6)
