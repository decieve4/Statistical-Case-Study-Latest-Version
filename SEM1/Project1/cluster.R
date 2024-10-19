source("SEM1/Project1/stylometryfunctions.R")
source("SEM1/Project1/Preprocessing.R")

library(ggplot2)
library(cluster)
library(factoextra)

# We Got Clean X from Preprocessing.R

# hierarchical cluster
dist_matrix <- dist(X, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "average")  # "complete", "average", "single"
fviz_dend(hclust_result,
          k = 12,  
          rect = TRUE,  
          rect_fill = FALSE,  
          rect_border = "jco",  
          show_labels = TRUE,  
          label_cols = NULL,  
          main = "Colored Dendrogram",
          cex = 0.8,  
          horiz = TRUE,
          yscale = "none"
)
