source("SEM1/Project2/stylometryfunctions.R")
library(randomForest)

# 1. load data

# 1.1 M

humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")

# 1.2 features

humanfeatures <- humanM$features
names(humanfeatures) <- rep(1, 110)
GPTfeatures <- GPTM$features
names(GPTfeatures) <- rep(2, 110)

# 2. run classifier

# 2.1 list all metrics

# 2.2 run on each topic

# 1:length(humanfeatures)

for (idx_topic in 1:1) {
  
  # a. split train and test data
  
  humanfeatures_for_train <- humanfeatures[-idx_topic]
  humanfeatures_for_test <- humanfeatures[idx_topic]
  GPTfeatures_for_train <- GPTfeatures[-idx_topic]
  GPTfeatures_for_test <- GPTfeatures[idx_topic]
  
  traindata <- c(humanfeatures_for_train, GPTfeatures_for_train)
  testdata <- c(humanfeatures_for_test, GPTfeatures_for_test)
  
  # b. init prediction list
  
  DApredictions <- NULL
  KNNpredictions <- NULL
  RFpredictions <- NULL
  truth <- NULL
  
  # c. start leave-one-out with each topic
  
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
      
      # true label
      truth <- c(truth, i)
    }
    
  }
  
}

# 3. summary result