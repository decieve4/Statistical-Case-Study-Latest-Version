# 加载必要的包和函数
source("SEM1/Project2/stylometryfunctions.R")
library(class)
library(randomForest)
library(caret)

set.seed(42)

# 1. 获取主题名称列表
topics <- list.files("SEM1/Project2/functionwords/functionwords/humanfunctionwords/")
num_topics <- length(topics)

# 2. 按主题循环进行分类分析
for (current_topic in 1:num_topics) {
  topic_name <- topics[current_topic]
  message(sprintf("正在处理主题 %s...", topic_name))
  
  # 2.1 获取当前主题的人类和GPT功能词频次数据
  human_topic_dir <- file.path("SEM1/Project2/functionwords/functionwords/humanfunctionwords", topic_name)
  GPT_topic_dir <- file.path("SEM1/Project2/functionwords/functionwords/GPTfunctionwords", topic_name)
  
  # 读取当前主题下所有的 .txt 文件
  human_files <- list.files(human_topic_dir, full.names = TRUE)
  GPT_files <- list.files(GPT_topic_dir, full.names = TRUE)
  
  # 如果当前主题下的人类或GPT文章数量太少（少于5篇），跳过该主题
  if (length(human_files) < 5 || length(GPT_files) < 5) {
    message(sprintf("主题 %s 数据量不足，跳过该主题", topic_name))
    next
  }
  
  # 2.2 将每个文件中的功能词频次读取到矩阵中
  human_topic_features <- do.call(rbind, lapply(human_files, function(file) {
    as.numeric(unlist(strsplit(readLines(file), split = ",")))
  }))
  
  GPT_topic_features <- do.call(rbind, lapply(GPT_files, function(file) {
    as.numeric(unlist(strsplit(readLines(file), split = ",")))
  }))
  
  # 2.3 合并人类和GPT数据
  dataset <- list(human_topic_features, GPT_topic_features)
  dataset.mat <- rbind(human_topic_features, GPT_topic_features)
  
  # 填充缺失值，使用列的均值
  dataset.mat[is.na(dataset.mat)] <- apply(dataset.mat, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  
  num_text <- nrow(human_topic_features) + nrow(GPT_topic_features)
  
  # 2.4 初始化交叉验证
  num_folds <- 5
  idx_total <- 1:num_text
  idx_folds <- vector("list", num_folds)
  
  # 2.5 分割数据集用于交叉验证
  for (i in 1:num_folds) {
    idx_folds[[i]] <- sample(idx_total, size = as.integer(num_text / num_folds), replace = FALSE)
    idx_total <- setdiff(idx_total, idx_folds[[i]])
  }
  
  # 2.6 初始化预测列表
  DApredictions <- NULL
  KNNpredictions <- NULL
  RFpredictions <- NULL
  truth <- NULL
  
  # 2.7 对每一折运行算法
  for (idx_fold in 1:num_folds) {
    idx <- idx_folds[[idx_fold]]
    idx_human <- idx[idx <= (num_text / 2)]
    idx_GPT <- idx[idx > (num_text / 2)] - (num_text / 2)
    
    # 测试数据
    cv_testdata <- dataset.mat[idx, ]
    
    # 训练数据
    cv_traindata <- dataset.mat[-idx, ]
    train_labels <- c(rep(1, nrow(human_topic_features) - length(idx_human)), 
                      rep(2, nrow(GPT_topic_features) - length(idx_GPT)))
    
    # 检查数据完整性
    if (any(is.na(cv_traindata)) || any(is.na(cv_testdata))) {
      message(sprintf("跳过第 %d 折，因数据中存在NA值。", idx_fold))
      next
    }
    
    # 2.8 判别分析
    tryCatch({
      DA_pred <- discriminantCorpus(list(cv_traindata[train_labels == 1, , drop = FALSE], 
                                         cv_traindata[train_labels == 2, , drop = FALSE]), cv_testdata)
      DApredictions <- c(DApredictions, DA_pred)
    }, error = function(e) {
      message(sprintf("判别分析错误，第 %d 折: %s", idx_fold, e$message))
      DApredictions <- c(DApredictions, rep(NA, length(idx)))
    })
    
    # 2.9 KNN 分类
    tryCatch({
      KNN_pred <- knn(train = cv_traindata, test = cv_testdata, cl = train_labels, k = 3)
      KNNpredictions <- c(KNNpredictions, as.integer(as.character(KNN_pred)))
    }, error = function(e) {
      message(sprintf("KNN 分类错误，第 %d 折: %s", idx_fold, e$message))
      KNNpredictions <- c(KNNpredictions, rep(NA, length(idx)))
    })
    
    # 2.10 随机森林
    tryCatch({
      RF_model <- randomForest(x = cv_traindata, y = as.factor(train_labels))
      RF_pred <- predict(RF_model, cv_testdata)
      RFpredictions <- c(RFpredictions, as.integer(as.character(RF_pred)))
    }, error = function(e) {
      message(sprintf("随机森林错误，第 %d 折: %s", idx_fold, e$message))
      RFpredictions <- c(RFpredictions, rep(NA, length(idx)))
    })
    
    truth_label_fold <- ifelse(idx <= (num_text / 2), 1, 2)
    truth <- c(truth, truth_label_fold)
  }
  
  # 3. 结果推断与可视化
  truth <- factor(truth, levels = c(1, 2))
  DApredictions <- factor(DApredictions, levels = c(1, 2))
  KNNpredictions <- factor(KNNpredictions, levels = c(1, 2))
  RFpredictions <- factor(RFpredictions, levels = c(1, 2))
  
  # 3.1 计算准确率
  message(sprintf("主题 %s 判别分析 (DA) 准确率: %.2f", topic_name, sum(DApredictions == truth, na.rm = TRUE) / length(na.omit(DApredictions))))
  message(sprintf("主题 %s KNN 准确率: %.2f", topic_name, sum(KNNpredictions == truth, na.rm = TRUE) / length(na.omit(KNNpredictions))))
  message(sprintf("主题 %s 随机森林 (RF) 准确率: %.2f", topic_name, sum(RFpredictions == truth, na.rm = TRUE) / length(na.omit(RFpredictions))))
  
  # 3.2 打印混淆矩阵
  message(sprintf("主题 %s 判别分析混淆矩阵:", topic_name))
  print(confusionMatrix(DApredictions, truth))
  
  message(sprintf("主题 %s KNN 混淆矩阵:", topic_name))
  print(confusionMatrix(KNNpredictions, truth))
  
  message(sprintf("主题 %s 随机森林混淆矩阵:", topic_name))
  print(confusionMatrix(RFpredictions, truth))
}
