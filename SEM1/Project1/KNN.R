# 加载自定义函数
source("./SEM1/Project1/stylometryfunctions.R")  # 调整路径

# 加载语料库
corpus <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

# 提取特征和标签
features <- corpus$features
authornames <- corpus$authornames

# 初始化训练数据、测试数据和测试标签
traindata <- corpus$features
testdata <- NULL
testlabels <- NULL  # 测试集的真实作者标签

# 为每个作者选择一个随机的书本作为测试集
for (i in 1:length(traindata)) {
  # 随机选择一个书本（对应一行数据）
  testind <- sample(1:nrow(traindata[[i]]), 1)
  
  # 将选择的书本添加到测试集
  testdata <- rbind(testdata, traindata[[i]][testind, ])
  testlabels <- c(testlabels, i)
  
  # 将该书本从训练集中移除
  traindata[[i]] <- traindata[[i]][-testind, , drop=FALSE]
}


# 初始化变量
predictions <- NULL
KNNpredictions <- NULL
truth <- NULL
features <- corpus$features



# 交叉验证循环
for (i in 1:length(features)) {
  for (j in 1:nrow(features[[i]])) {
    
    # 将当前行的特征作为测试数据
    testdata <- matrix(features[[i]][j,], nrow=1)
    
    # 将 features 复制为 traindata，并删除当前行的数据以避免泄漏
    traindata <- features
    traindata[[i]] <- traindata[[i]][-j, , drop=FALSE]
    
    if (nrow(traindata[[i]]) == 0){
      traindata <- traindata[-i]
    }
    
    # 使用 discriminantCorpus 进行分类
    
    pred <- discriminantCorpus(traindata, testdata)
    predictions <- c(predictions, pred)  # 将预测结果追加到 predictions
    
    # 使用 KNNCorpus 进行 KNN 分类
    
    pred <- KNNCorpus(traindata, testdata)
   
    KNNpredictions <- c(KNNpredictions, pred)  # 将KNN预测结果追加到 KNNpredictions
    
    # 记录真实类别
    truth <- c(truth, i)
  }
}

sum(predictions==truth)/length(truth)

sum(KNNpredictions==truth)/length(truth)

confusionMatrix(as.factor(predictions), as.factor(truth))


