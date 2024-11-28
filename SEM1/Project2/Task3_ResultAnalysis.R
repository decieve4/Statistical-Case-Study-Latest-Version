library(tidyr) # lib

# Load Data
Task3_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task3_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(Task3_Result) <- c("topic_name", "DA", "KNN", "RF") # for pivot long

# Load Baseline Result
Task0_Result <- read.csv("./SEM1/Project2/Figures/baseline_accuracy_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# long format for ggplot
Task3_long_results <- pivot_longer(Task3_Result, 
                             cols = colnames(Task3_Result)[-1],
                             names_to = "Model",
                             values_to = "Accuracy")

fig <- ggplot(data = Task3_long_results, aes(x = Model, y = Accuracy, fill = Model)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Accuracy Comparison Boxplot") + 
  theme_bw()

ggsave("./SEM1/Project2/Figures/Task3 Model Accuracy Distribution Boxplot.pdf", plot = fig, width = 8, height = 6)
