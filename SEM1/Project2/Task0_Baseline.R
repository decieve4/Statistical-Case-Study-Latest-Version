source("SEM1/Project2/reducewords.R")
source("SEM1/Project2/stylometryfunctions.R")
library(randomForest)

# 1. load data
# 1.1 M
humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")
# 1.2 features
humanfeatures <- humanM$features
GPTfeatures <- GPTM$features
# 1.3 change list to big matrix

# 1.3* sample 5 topics

# 1.4 combine human and GPT to be a list with index 1 and 2
features <- c(humannfeatures, GPTfeatures)

# 2. run classifier
traindata <- features

DApredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL
truth <- NULL

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
