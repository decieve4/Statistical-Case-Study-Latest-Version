source("SEM1/Project1/stylometryfunctions.R")
# source("SEM1/Project1/bookscolors.R")

library(ggplot2)
library(cluster)
library(factoextra)

M <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

# data
X <- do.call(rbind, M$features)
rownames(X) <- unlist(M$booknames)

X_normalized <- X / rowSums(X)

# data without Frankenstein
X_no_frankenstein <- do.call(rbind, M$features[-9])
rownames(X_no_frankenstein) <- unlist(M$booknames[-9])

# hierarchical cluster
dist_matrix <- dist(X_normalized, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "average")  # "complete", "average", "single"
fviz_dend(hclust_result,
          k = 12,  # 设置为 12 个簇
          rect = TRUE,  # 绘制矩形框线
          rect_fill = FALSE,  # 填充框线颜色
          rect_border = "jco",  # 使用 jco 配色方案
          show_labels = TRUE,  # 显示标签
          label_cols = NULL,  # 标签颜色与簇对应
          main = "Colored Dendrogram",
          cex = 0.6,  # 调整字体大小
          horiz = TRUE,  # 垂直显示
          yscale = "none"
)

# # kmeans cluster
# kmeans_result <- kmeans(X, centers = 12)
# fviz_cluster(kmeans_result, data = X,
#              ellipse.type = "convex",  # 选择椭圆形包围簇
#              geom = "point",  # 显示簇的点
#              show.clust.cent = TRUE,  # 显示簇的中心点
#              main = "K-means Clustering",
#              repel = TRUE,  # 标签避免重叠
#              ggtheme = theme_minimal())
