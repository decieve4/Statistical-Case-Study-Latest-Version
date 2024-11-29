# Load necessary packages
library(ggplot2)
library(tidyr)
library(dplyr)  # Load dplyr for group_by function

# Load the data for overall accuracy
results_long <- read.csv("./SEM1/Project2/Experiment_Result/Task2_overall_accuracy.csv")
baseline_results <- read.csv("./SEM1/Project2/Experiment_Result/Baseline_Result.csv")

# Ensure the loaded data has correct column names
colnames(results_long) <- c("Model", "Accuracy")

# Transform baseline data for ggplot2
baseline_long <- pivot_longer(baseline_results, cols = c("DA_accuracy", "KNN_accuracy", "RF_accuracy"), 
                              names_to = "Model", values_to = "Baseline_Accuracy")

# Filter out models that have less than 2 data points (to avoid errors in violin plot)
filtered_results <- results_long %>%
  group_by(Model) %>%
  filter(n() >= 2)

# Plot accuracy distribution using violin plots
accuracy_plot <- ggplot(filtered_results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_hline(data = baseline_long, aes(yintercept = Baseline_Accuracy, color = Model), 
             linetype = "dashed", size = 1) +  # Add dashed lines for baselines
  scale_color_manual(values = c("red", "green", "blue")) +  # Set color for baseline lines
  theme_bw() + 
  labs(title = "Accuracy Distribution of Different Models for Each Topic with Baseline",
       x = "Model Type", y = "Accuracy", fill = "Model Type") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 14),  # Set global font size
        axis.title = element_text(size = 16),  # Adjust axis title font size
        axis.text = element_text(size = 12),  # Adjust axis tick labels font size
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12))  # Adjust legend text font size

# Display accuracy distribution plot
print(accuracy_plot)

# Save violin plot using ggsave() and ensure the consistent output format
ggsave("./SEM1/Project2/Figures/Task2_accuracy_distribution_with_baseline.pdf", 
       plot = accuracy_plot, 
       width = 8, 
       height = 6,
       bg = "white")
