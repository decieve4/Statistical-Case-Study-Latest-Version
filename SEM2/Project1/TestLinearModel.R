# 加载必要的库
library(ggplot2)

# 设置随机种子，确保结果可重现
set.seed(42)

# 生成数据：y = 3 + 2 * x + 随机噪声
n <- 1000
x <- exp(rnorm(n))
y <- exp(3 + 2 * log(x)) + rnorm(n)

# 将数据转化为数据框
data <- data.frame(x = log(x), y = log(y))

# 使用ggplot2绘图，添加回归线和标准误差带
ggplot(data, aes(x = log(x), y = log(y))) +
  geom_point() + # 绘制数据点
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, level = 0.95, color = "black") + # 添加线性回归拟合线和标准误差带
  theme_minimal() +
  labs(title = "Linear Regression with SE Band", x = "X", y = "Y")

model = lm(data = data, log(y) ~ log(x))
summary(model)

par(mfrow = c(2, 2))
plot(model)


x[1]
y[1]

