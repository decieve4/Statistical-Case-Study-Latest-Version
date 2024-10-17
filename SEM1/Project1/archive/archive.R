library(cluster)

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