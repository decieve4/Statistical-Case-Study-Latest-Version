library(ggplot2)
library(ggcorrplot)
source("SEM2/Project2/data_preprocessing.R")
cor_matrix <- cor(data[, c("demand_gross", "temp", "TO", "TE", "wind", "solar_S")], use = "complete.obs")

p <- ggcorrplot(cor_matrix, 
                method = "square",
                type = "full",
                lab = TRUE,
                lab_size = 4,
                digits = 2,
                colors = c("#2166AC", "white", "#B2182B"),
                outline.col = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
print(p)
