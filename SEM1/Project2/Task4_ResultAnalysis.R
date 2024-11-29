library(ggplot2)
library(tidyr)

# Load Baseline Result
Task0_Result <- read.csv("./SEM1/Project2/Experiment_Result/Baseline_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_10_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_10_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_20_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_20_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_30_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_30_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_40_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_40_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_50_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_50_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Task4_60_Result <- read.csv("./SEM1/Project2/Experiment_Result/Task4_60_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

Final_Result <- rbind(Task0_Result, Task4_10_Result, Task4_20_Result, Task4_30_Result, Task4_40_Result, Task4_50_Result, Task4_60_Result)

colnames(Final_Result) <- c("Dataset", "DA", "KNN", "RF")

Result_long <- pivot_longer(Final_Result, 
                            cols = colnames(Final_Result)[-1], 
                            names_to = "Model", 
                            values_to = "Accuracy") 

Baseline_long <- Result_long[1:3, ]
Result_long <- Result_long[-c(1, 2, 3), ]

fig <- ggplot(Result_long, aes(x = Dataset, y = Accuracy, color = Model, group = Model)) +
  geom_line(linewidth = 1.5) +                # Plot the lines connecting the points for each model
  geom_point(size = 4) +               # Add points to show the accuracy values
  labs(title = "Accuracy of Different Models Across Different Dimension",  # Plot title
       x = "Dateset",  # X-axis label
       y = "Accuracy") +  # Y-axis label
  scale_color_brewer(palette = "Set2") + 
  geom_hline(data = Baseline_long, 
             aes(yintercept = Accuracy, color = Model), 
             linetype = "dashed", 
             linewidth = 1) +   # Add horizontal dashed lines for Baseline
  theme_bw() + 
  theme(
    text = element_text(size = 14),  # Set global font size
    axis.title = element_text(size = 16),  # Adjust axis title font size
    axis.text = element_text(size = 12),  # Adjust axis tick labels font size
    legend.title = element_text(size = 14),  # Adjust legend title font size
    legend.text = element_text(size = 12),  # Adjust legend text font size
    plot.title = element_text(size = 18)  # Adjust plot title font size
  )

ggsave("./SEM1/Project2/Figures/Task4 Acc Lineplot.pdf", width = 8, height = 6)
