source("SEM1/Project2/stylometryfunctions.R")

library(ggplot2)
library(tidyr)

set.seed(42)

# 1. load data
# 1.1 M
# humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
# GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")
# 1.2 features
humanfeatures <- humanM$features
GPTfeatures <- GPTM$features
# 1.3 make big mat
humanfeatures.mat <- do.call(rbind, humanfeatures)
GPTfeatures.mat <- do.call(rbind, GPTfeatures)
# 1.4 remanme the big mat
colnames_70 <- c("a", "all", "also", "an", "and", "any", "are", "as", "at", 
                 "be", "been", "but", "by", "can", "do", "down", "even", "every", 
                 "for", "from", "had", "has", "have", "her", "his", "if", "in", 
                 "into", "is", "it", "its", "may", "more", "must", "my", "no", 
                 "not", "now", "of", "on", "one", "only", "or", "our", "shall", 
                 "should", "so", "some", "such", "than", "that", "the", "their", 
                 "then", "there", "things", "this", "to", "up", "upon", "was", 
                 "were", "what", "when", "which", "who", "will", "with", "would", 
                 "your", "Total Count")
colnames(humanfeatures.mat) <- colnames_70
colnames(GPTfeatures.mat) <- colnames_70

# 2. dimension reduction

X <- rbind(humanfeatures.mat, GPTfeatures.mat)
X <- X[, -ncol(X)] # we want to keep Total Count
Cov_X <- cor(X)

Cov_X_melted <- melt(Cov_X)
fig <- ggplot(Cov_X_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./SEM1/Project2/Figures/Task4 CovX Original Heatmap.pdf", plot = fig, width = 10, height = 8)

# var list
variances <- apply(X, 2, var)
# sds <- apply(X, 2, sd)

# num of dimension reduction
k <- 60
remove_idx_list <- NULL
  
# alg
for (i in 1:k) {
  
  cor_matrix <- abs(Cov_X)
  diag(cor_matrix) <- 0
  
  # highest correlation pair
  max_corr_idx <- which(cor_matrix == max(cor_matrix), arr.ind = TRUE)
  feature1 <- max_corr_idx[1, 1]
  feature2 <- max_corr_idx[1, 2]
  
  # search the vars of each feature
  var1 <- variances[feature1]
  var2 <- variances[feature2]
  
  # delete the one has lower var (less info inside this feature)
  if (var1 < var2) {
    X <- X[, -feature1] # delete the col
    Cov_X <- cor(X)  # update CovX
    variances <- variances[-feature1]  # update var list
    feature <- feature1
    remove_idx_list <- c(remove_idx_list, feature)
  } else {
    X <- X[, -feature2]
    Cov_X <- cor(X)
    variances <- variances[-feature2]  # 更新方差
    feature <- feature2
    remove_idx_list <- c(remove_idx_list, feature)
  }
  
  # print
  cat("Iteration", i, ": Removed feature", feature1, "or", feature2, ", remove", feature, "\n")
}

Cov_X_melted <- melt(Cov_X)
fig <- ggplot(Cov_X_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

ggsave("./SEM1/Project2/Figures/Task4 CovX Reduction 60 Heatmap.pdf", plot = fig, width = 8, height = 6)

# 3. Final Dataset: combine human and GPT to be a list with index 1 (human) and 2 (GPT)
remove_idx_list
colnames_70[remove_idx_list]
humanfeatures.mat <- humanfeatures.mat[, -remove_idx_list]
GPTfeatures.mat <- GPTfeatures.mat[, -remove_idx_list]
features <- list(humanfeatures.mat, GPTfeatures.mat)

start_time <- proc.time()

# # 4. run classifier
# # 4.1 MAKE SURE TO USE CORRECT DATA
# dataset <- features
# dataset.mat <- rbind(features[[1]], features[[2]])
# num_text <- nrow(features[[1]]) + nrow(features[[2]])
# 
# # 4.2 init prediction list
# DApredictions <- NULL
# KNNpredictions <- NULL
# RFpredictions <- NULL
# truth <- NULL
# 
# # 4.3 start leave-one-out or Cross-Validation
# # 4.3.1 try cross-validation, sample idx for each fold
# idx_total <- 1:num_text
# num_folds <- 5
# idx_folds <- vector("list", num_folds)
# for (i in 1:num_folds) {
#   idx_folds[[i]] <- sample(idx_total, size = as.integer(num_text / num_folds), replace = FALSE)
#   idx_total <- setdiff(idx_total, idx_folds[[i]])
# }
# # 4.3.2 run algorithm
# for (idx_fold in 1:num_folds){
#   
#   # a. get idx
#   
#   idx <- idx_folds[[idx_fold]]
#   idx_human <- idx[idx <= (num_text / 2)]
#   idx_GPT <- idx[idx > (num_text / 2)] - (num_text / 2)
#   
#   # sample testdata
#   
#   cv_testdata <- dataset.mat[idx, ]
#   
#   # the rest of data is train data
#   
#   cv_traindata <- dataset
#   
#   # human
#   
#   cv_traindata[[1]] <- cv_traindata[[1]][-idx_human, ]
#   
#   # GPT
#   
#   cv_traindata[[2]] <- cv_traindata[[2]][-idx_GPT, ]
#   
#   # fit da with traindatq, and validate with testdata
#   
#   DA_pred <- discriminantCorpus(cv_traindata, cv_testdata)
#   DApredictions <- c(DApredictions, DA_pred)  # save result
#   
#   # fit knn with traindatq, and validate with testdata
#   
#   KNN_pred <- KNNCorpus(cv_traindata, cv_testdata)
#   KNNpredictions <- c(KNNpredictions, KNN_pred)  # save result
#   
#   # fit rf with traindatq, and validate with testdata
#   
#   RF_pred <- randomForestCorpus(cv_traindata, cv_testdata)
#   RFpredictions <- c(RFpredictions, RF_pred)  # save result
#   
#   # true label for this fold's testdata
#   
#   truth_label_fold <- ifelse(idx <= (num_text / 2), 1, 2)
#   truth <- c(truth, truth_label_fold) # save result
#   
# }
# 
# end_time <- proc.time()
# message("Run Time:")
# print(end_time - start_time)
# 
# # 5. inference and visualize results
# # 5.1 convert numeric -> factor
# truth <- factor(truth, levels = sort(unique(truth)))
# DApredictions <- factor(DApredictions, levels = levels(truth))
# KNNpredictions <- factor(KNNpredictions, levels = levels(truth))
# RFpredictions <- factor(RFpredictions, levels = levels(truth))
# # 5.2 sum bool factor
# message("Discriminant Analysis (DA) Accuracy: ", sum(DApredictions==truth)/length(truth))
# message("KNN Accuracy: ", sum(KNNpredictions==truth)/length(truth))
# message("Random Forest (RF) Accuracy: ", sum(RFpredictions==truth)/length(truth))
# # 5.3 print 
# message("Confusion Matrix for Discriminant Analysis:")
# print(confusionMatrix(DApredictions, truth))
# message("Confusion Matrix for KNN:")
# print(confusionMatrix(KNNpredictions, truth))
# message("Confusion Matrix for Random Forest:")
# print(confusionMatrix(RFpredictions, truth))
# 
# Task4_Result <- data.frame(
#   topic = "Filtered 60",
#   DA_accuracy = sum(DApredictions == truth) / length(truth),
#   KNN_accuracy = sum(KNNpredictions == truth) / length(truth),
#   RF_accuracy = sum(RFpredictions == truth) / length(truth)
# )
# 
# write.csv(Task4_Result, file = "./SEM1/Project2/Experiment_Result/Task4_60_Result.csv", row.names = FALSE)
