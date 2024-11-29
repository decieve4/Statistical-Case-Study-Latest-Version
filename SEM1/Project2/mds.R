source("SEM1/Project2/stylometryfunctions.R")
humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")

# 1.2 features

humanfeatures <- humanM$features
GPTfeatures <- GPTM$features
features <- c(humanfeatures,GPTfeatures)
# 1.3 change list to big matrix

#humanfeatures.mat <- do.call(rbind, humanfeatures)
#GPTfeatures.mat <- do.call(rbind, GPTfeatures)
#mat<-rbind(humanfeatures.mat,GPTfeatures.mat)


#features <- list(humanfeatures.mat, GPTfeatures.mat)

##ALL
x<-NULL
for (i in 1:length(features)){
  x<-rbind(x,apply(features[[i]],2,sum))
}

for (i in 1:nrow(x)){
  x[i,]<-x[i,]/sum(x[i,])
}
for(j in i:ncol(x)){
  x[,j]<-(x[,j]-mean(x[,j]))/sd(x[,j])
}
d<-dist(x)
pts<-cmdscale(d)
#plot(pts,type='n')
#title(main="Multidimensional Scaling Plot For All Authors")
#text(pts[,1],pts[,2],cex=0.8)
# 分组索引
group <- c(rep("blue", 110), rep("red", 110))  # 前 110 个为蓝色，后 110 个为红色

# 绘图
plot(pts[, 1], pts[, 2], type = "n",  # 初始化空图
     xlab = "pts[,1]", ylab = "pts[,2]",
     main = "Multidimensional Scaling Plot of All Essays Based on Topic")

# 添加前 110 个点（蓝色）
points(pts[1:110, 1], pts[1:110, 2], col = "blue", pch = 19)

# 添加后 110 个点（红色）
points(pts[111:220, 1], pts[111:220, 2], col = "red", pch = 19)

# 添加文本标签（蓝色和红色分开）
#text(pts[1:110, 1], pts[1:110, 2], labels = 1:110, col = "blue", cex = 0.8)
#text(pts[111:220, 1], pts[111:220, 2], labels = 111:220, col = "red", cex = 0.8)
legend("topright",               # 图例位置，可调整为 "topleft", "bottomright" 等
       legend = c("Human", "ChatGPT"),  # 图例内容
       col = c("blue", "red"),   # 图例中点的颜色
       pch = 19,                 # 图例中点的形状
       cex = 0.8) 
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

# 加载 ggplot2
library(ggplot2)

# 创建数据框
# 假设 pts 是一个 220 行 2 列的矩阵
data <- data.frame(
  x = pts[, 1],                     # x 坐标
  y = pts[, 2],                     # y 坐标
  group = rep(c("Human", "ChatGPT"), each = 110)  # 更改分组名称
)

# 绘图
p = ggplot(data, aes(x = x, y = y, color = group)) +  # 按分组设置颜色
  geom_point(size = 3) +                         # 绘制点，调整点大小
  labs(
    x = "Dimension 1",                                       # x轴标签
    y = "Dimension 2",                                       # y轴标签
    color = "Author"                                         # 图例标题
  ) +
  labs() + 
  theme_bw() + 
  theme(
    text = element_text(size = 14),  # Set global font size
    axis.title = element_text(size = 16),  # Adjust axis title font size
    axis.text = element_text(size = 12),  # Adjust axis tick labels font size
    legend.title = element_text(size = 14),  # Adjust legend title font size
    legend.text = element_text(size = 12),  # Adjust legend text font size
    plot.title = element_text(size = 18)  # Adjust plot title font size
  )+
  scale_color_brewer(palette = "Set2") # 使用简洁主题

ggsave("./SEM1/Project2/Figures/MDSplot.pdf", plot = p, width = 8, height = 6)
