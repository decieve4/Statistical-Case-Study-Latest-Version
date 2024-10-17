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

# # kmeans cluster
# kmeans_result <- kmeans(X, centers = 12)
# fviz_cluster(kmeans_result, data = X,
#              ellipse.type = "convex",  
#              geom = "point",  
#              show.clust.cent = TRUE,  
#              main = "K-means Clustering",
#              repel = TRUE,  
#              ggtheme = theme_minimal())

# # data
# X <- do.call(rbind, M$features)
# rownames(X) <- unlist(M$booknames)
# 
# X_normalized <- X / rowSums(X)
# 
# # data without Frankenstein
# X_no_frankenstein <- do.call(rbind, M$features[-9])
# rownames(X_no_frankenstein) <- unlist(M$booknames[-9])
