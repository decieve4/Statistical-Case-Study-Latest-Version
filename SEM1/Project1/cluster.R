source("SEM1/Project1/stylometryfunctions.R")
source("SEM1/Project1/bookscolors.R")

library(ggplot2)
library(cluster)
library(factoextra)

M <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

# data
X <- do.call(rbind, M$features)
rownames(X) <- unlist(M$booknames)

# data without Frankenstein
X_no_frankenstein <- do.call(rbind, M$features[-9])
rownames(X_no_frankenstein) <- unlist(M$booknames[-9])

# color_vector <- c(
#   rep("#E41A1C", 3),  # 作者 1 (Stoker)
#   rep("#377EB8", 5),  # 作者 2 (Brown)
#   rep("#4DAF4A", 1),  # 作者 3 (Shelley)
#   rep("#984EA3", 6),  # 作者 4 (MShelley)
#   rep("#FF7F00", 2),  # 作者 5 (Wollstonecraft)
#   rep("#FFFF33", 2),  # 作者 6 (PShelley)
#   rep("#A65628", 3),  # 作者 7 (PShelleyPoet)
#   rep("#F781BF", 4),  # 作者 8 (Peacock)
#   rep("#999999", 1),  # 作者 9 (Classify Frankenstein)
#   rep("#66C2A5", 6),  # 作者 10 (Scott)
#   rep("#FC8D62", 5),  # 作者 11 (Godwin)
#   rep("#8DA0CB", 1)   # 作者 12 (Polidori)
# )

# hierarchical cluster
dist_matrix <- dist(X, method = "euclidean")
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

# kmeans cluster
kmeans_result <- kmeans(X, centers = 12)
fviz_cluster(kmeans_result, data = X,
             ellipse.type = "convex",  # 选择椭圆形包围簇
             geom = "point",  # 显示簇的点
             show.clust.cent = TRUE,  # 显示簇的中心点
             main = "K-means Clustering",
             repel = TRUE,  # 标签避免重叠
             ggtheme = theme_minimal())
