library(tidyr) # lib
library(ggplot2)

# Load Data
Task3_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task3_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(Task3_Result) <- c("topic_name", "DA", "KNN", "RF") # for pivot long

# Load Baseline Result
Task0_Result <- read.csv("./SEM1/Project2/Figures/baseline_accuracy_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(Task0_Result) <- c("topic_name", "DA", "KNN", "RF") # for pivot long

# long format for ggplot
Task3_long_results <- pivot_longer(Task3_Result, 
                             cols = colnames(Task3_Result)[-1],
                             names_to = "Model",
                             values_to = "Accuracy")

Task0_long_results <- pivot_longer(Task0_Result,
                                   cols = colnames(Task3_Result)[-1],
                                   names_to = "Model",
                                   values_to = "Accuracy")

fig <- ggplot(data = Task3_long_results, aes(x = Model, y = Accuracy, fill = Model)) + 
  geom_violin(trim = TRUE, alpha = 0.5, bw = 0.01) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(data = Task0_long_results,
             aes(yintercept = Accuracy, color = Model),
             linetype = "dashed", linewidth = 1) +   # Add horizontal dashed lines for Baseline
  scale_color_brewer(palette = "Set2") + 
  labs(title = "Model Accuracy Comparison Boxplot") + 
  theme_bw() +
  theme(
    text = element_text(size = 14),  # Set global font size
    axis.title = element_text(size = 16),  # Adjust axis title font size
    axis.text = element_text(size = 12),  # Adjust axis tick labels font size
    legend.title = element_text(size = 14),  # Adjust legend title font size
    legend.text = element_text(size = 12),  # Adjust legend text font size
    plot.title = element_text(size = 18)  # Adjust plot title font size
  )

ggsave("./SEM1/Project2/Figures/Task3 Model Accuracy Distribution Boxplot.pdf", plot = fig, width = 8, height = 6)
