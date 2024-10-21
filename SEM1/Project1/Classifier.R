# 加载自定义函数
source("./SEM1/Project1/stylometryfunctions.R")  # 调整路径
source("SEM1/Project1/Preprocessing.R")
library(randomForest)

# 加载语料库
corpus <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

# 提取特征和标签
features <- corpus$features
authornames <- corpus$authornames

# ### change features with clean data
# # Cleaned matrix `X`, assumed to be the combined cleaned data
# X_cleaned <- X  # Assuming `X` is already cleaned and ready for use
# 
# # Determine the original structure of `features` (number of rows for each author)
# rows_per_author <- sapply(features, nrow)
# 
# # Split the cleaned matrix `X_cleaned` back into a list
# new_features <- split(X_cleaned, rep(1:length(rows_per_author), rows_per_author))
# 
# # Ensure each element is correctly reshaped into a matrix
# for (i in seq_along(new_features)) {
#   new_features[[i]] <- matrix(new_features[[i]], nrow = rows_per_author[i], byrow = TRUE)
# }
# 
# # Assign the new list back to `features`
# features <- new_features
# rm(new_features)
# ###

# 确保《Frankenstein》作为测试集，移除它的训练数据
traindata <- features[-9]  # 移除第9个作家（Unknown - Frankenstein）
authornames <- authornames[-9]
testdata <- matrix(features[[9]], nrow=1)  # 将Frankenstein作为测试数据
testlabels <- 9  # 将Unknown标记为第9个作家（Frankenstein）

# 初始化变量
DApredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL  # Random Forest 预测结果
truth <- NULL

# 交叉验证循环（针对其他11位作家）
for (i in 1:length(traindata)) {
  for (j in 1:nrow(traindata[[i]])) {
    
    # 将当前行的特征作为测试数据
    cv_testdata <- matrix(traindata[[i]][j,], nrow=1)
    
    # 将 traindata 复制，删除当前行的数据以避免泄漏
    cv_traindata <- traindata
    cv_traindata[[i]] <- cv_traindata[[i]][-j, , drop=FALSE]
    
    # 防止traindata出现空集
    if (nrow(cv_traindata[[i]]) == 0){
      cv_traindata <- cv_traindata[-i]
    }
    
    # 使用 discriminantCorpus 进行分类
    DA_pred <- discriminantCorpus(cv_traindata, cv_testdata)
    DApredictions <- c(DApredictions, DA_pred)  # 将预测结果追加到 predictions
    
    # 使用 KNNCorpus 进行 KNN 分类
    KNN_pred <- KNNCorpus(cv_traindata, cv_testdata)
    KNNpredictions <- c(KNNpredictions, KNN_pred)  # 将KNN预测结果追加到 KNNpredictions
    
    # 使用 randomForestCorpus 进行分类
    RF_pred <- randomForestCorpus(cv_traindata, cv_testdata)
    RFpredictions <- c(RFpredictions, RF_pred)  # 将 Random Forest 预测结果追加到 RFpredictions
    
    # 记录真实类别
    truth <- c(truth, i)
  }
}

# 确保 predictions 和 truth 的因子水平一致
truth <- factor(truth, levels = sort(unique(truth)))
DApredictions <- factor(DApredictions, levels = levels(truth))
KNNpredictions <- factor(KNNpredictions, levels = levels(truth))
RFpredictions <- factor(RFpredictions, levels = levels(truth))

# 打印交叉验证的结果
cat("Discriminant Analysis (DA) Accuracy: ", sum(DApredictions==truth)/length(truth), "\n")
cat("KNN Accuracy: ", sum(KNNpredictions==truth)/length(truth), "\n")
cat("Random Forest (RF) Accuracy: ", sum(RFpredictions==truth)/length(truth), "\n")

# 混淆矩阵
cat("Confusion Matrix for Discriminant Analysis: \n")
print(confusionMatrix(as.factor(DApredictions), as.factor(truth)))

cat("Confusion Matrix for KNN: \n")
print(confusionMatrix(as.factor(KNNpredictions), as.factor(truth)))

cat("Confusion Matrix for Random Forest: \n")
print(confusionMatrix(as.factor(RFpredictions), as.factor(truth)))

# 将Frankenstein作为测试集进行分类
DA_frankenstein_pred <- discriminantCorpus(traindata, testdata)
KNN_frankenstein_pred <- KNNCorpus(traindata, testdata)
RF_frankenstein_pred <- randomForestCorpus(traindata, testdata)

# 打印《Frankenstein》的分类结果
cat("Discriminant Analysis Prediction for Frankenstein: ", DA_frankenstein_pred, authornames[DA_frankenstein_pred], "\n")
cat("KNN Prediction for Frankenstein: ", KNN_frankenstein_pred, authornames[KNN_frankenstein_pred], "\n")
cat("Random Forest Prediction for Frankenstein: ", RF_frankenstein_pred, authornames[RF_frankenstein_pred], "\n")

