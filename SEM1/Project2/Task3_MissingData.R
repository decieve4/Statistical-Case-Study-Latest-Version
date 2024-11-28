source("SEM1/Project2/stylometryfunctions.R")

# 1. load data
# 1.1 M
humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")
# 1.2 features
humanfeatures <- humanM$features
GPTfeatures <- GPTM$features

# 2. run classifier
# 2.1 list all metrics
DApredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL
truth <- NULL
# 2.2 metric saver
topic_names <- list.files("SEM1/Project2/functionwords/titles/")
results_list <- list()
# 2.3 run on each topic, fit model with data without topic_i, test model on topic_i, compare with baseline as model fit with "almost" leave-one-out.
num_topic <- length(humanfeatures)

start_time <- proc.time()

for (idx_topic in 1:num_topic) {
  
  # a. split train (data without idx_topic) and test data (idx_topic)
  humanfeatures_for_train <- humanfeatures[-idx_topic]
  humanfeatures_for_test <- humanfeatures[idx_topic]
  GPTfeatures_for_train <- GPTfeatures[-idx_topic]
  GPTfeatures_for_test <- GPTfeatures[idx_topic]
  # i) testdata in matrix
  cv_testdata <- rbind(humanfeatures_for_test[[1]], GPTfeatures_for_test[[1]])
  # *i) label testdata
  num_row <- nrow(humanfeatures_for_test[[1]])
  truth_label_topic <- c(rep(1, num_row), rep(2, num_row))
  truth <- c(truth, truth_label_topic) # save result
  # ii) traindata in list(mat1, mat2)
  humanfeatures_for_train.mat <- do.call(rbind, humanfeatures_for_train)
  GPTfeatures_for_train.mat <- do.call(rbind, GPTfeatures_for_train)
  cv_traindata <- list(humanfeatures_for_train.mat, GPTfeatures_for_train.mat)
  
  # b. start fit and test on unseen idx_topic
  # # fit da with traindatq, and validate with testdata
  # DA_pred <- discriminantCorpus(cv_traindata, cv_testdata)
  # DApredictions <- c(DApredictions, DA_pred)  # save result
  # # fit knn with traindatq, and validate with testdata
  # KNN_pred <- KNNCorpus(cv_traindata, cv_testdata)
  # KNNpredictions <- c(KNNpredictions, KNN_pred)  # save result
  # fit rf with traindatq, and validate with testdata
  RF_pred <- randomForestCorpus(cv_traindata, cv_testdata)
  RFpredictions <- c(RFpredictions, RF_pred)  # save result
  
  # c. evaluate the topic performance
  # i) make sure the factor level
  truth_label_topic <- factor(truth_label_topic, levels = c(1, 2))
  # DA_pred <- factor(DA_pred, levels = c(1, 2))
  # KNN_pred <- factor(KNN_pred, levels = c(1, 2))
  RF_pred <- factor(RF_pred, levels = c(1, 2))
  # ii) metric: accuracy
  # acc_DA <- sum(DA_pred == truth_label_topic)/length(truth_label_topic)
  # acc_KNN <- sum(KNN_pred == truth_label_topic)/length(truth_label_topic)
  acc_RF <- sum(RF_pred == truth_label_topic)/length(truth_label_topic)
  # # iii) save result
  # results_list[[idx_topic]] <- data.frame(
  #   topic_name = topic_names[idx_topic],
  #   DA_accuracy = acc_DA,
  #   KNN_accuracy = acc_KNN,
  #   RF_accuracy = acc_RF
  # )
  
}

end_time <- proc.time()
message("RF Run Time:")
print(end_time - start_time)

# # 3. summary result in total
# 
# # 3.*
# results_df <- do.call(rbind, results_list)
# write.csv(results_df, file = "./SEM1/Project2/Experiment_Result/Task3_results.csv", row.names = FALSE)
# 
# # 3.1 num -> factor
# truth <- factor(truth, levels = sort(unique(truth)))
# DApredictions <- factor(DApredictions, levels = levels(truth))
# KNNpredictions <- factor(KNNpredictions, levels = levels(truth))
# RFpredictions <- factor(RFpredictions, levels = levels(truth))
# # 3.2 overall accuracy
# message("Discriminant Analysis (DA) Accuracy: ", sum(DApredictions==truth)/length(truth))
# message("KNN Accuracy: ", sum(KNNpredictions==truth)/length(truth))
# message("Random Forest (RF) Accuracy: ", sum(RFpredictions==truth)/length(truth))
# # 3.3 confusion matrix
# message("Confusion Matrix for Discriminant Analysis (DA):")
# print(confusionMatrix(DApredictions, truth))
# message("Confusion Matrix for KNN:")
# print(confusionMatrix(KNNpredictions, truth))
# message("Confusion Matrix for Random Forest (RA):")
# print(confusionMatrix(RFpredictions, truth))
