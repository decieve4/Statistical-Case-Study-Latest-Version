# 加载必要的包和函数
source("SEM1/Project2/stylometryfunctions.R")
library(class)
library(randomForest)
library(caret)

set.seed(42)

# 1. load data

# 1.1 M - Load human and GPT data
humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")

# 1.2 features
humanfeatures <- humanM$features
GPTfeatures <- GPTM$features

# 1.3 change list to big matrix
humanfeatures.mat <- do.call(rbind, humanfeatures)
GPTfeatures.mat <- do.call(rbind, GPTfeatures)

# 1.4 combine human and GPT to be a list with index 1 (human) and 2 (GPT)
features <- list(humanfeatures.mat, GPTfeatures.mat)

# Fill missing values with column means
for (i in 1:length(features)) {
  features[[i]][is.na(features[[i]])] <- colMeans(features[[i]], na.rm = TRUE)[col(features[[i]])[is.na(features[[i]])]]
}

# 2. run classifier

# 2.1 Combine dataset for cross-validation
dataset <- features
dataset.mat <- rbind(features[[1]], features[[2]])
num_text <- nrow(features[[1]]) + nrow(features[[2]])

# 2.2 Initialize prediction lists
DApredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL
truth <- NULL

# 2.3 Cross-validation
num_folds <- 5
idx_total <- 1:num_text
idx_folds <- vector("list", num_folds)

# Divide data into folds
for (i in 1:num_folds) {
  idx_folds[[i]] <- sample(idx_total, size = as.integer(num_text / num_folds), replace = FALSE)
  idx_total <- setdiff(idx_total, idx_folds[[i]])
}

# 2.4 Run algorithm for each fold
for (idx_fold in 1:num_folds) {
  # Get the indices for the current fold
  idx <- idx_folds[[idx_fold]]
  idx_human <- idx[idx <= (num_text / 2)]
  idx_GPT <- idx[idx > (num_text / 2)] - (num_text / 2)
  
  # Test data
  cv_testdata <- dataset.mat[idx, ]
  
  # Train data
  cv_traindata <- dataset
  cv_traindata[[1]] <- cv_traindata[[1]][-idx_human, , drop = FALSE]
  cv_traindata[[2]] <- cv_traindata[[2]][-idx_GPT, , drop = FALSE]
  
  # Check for missing values in training and testing data
  if (any(is.na(cv_traindata[[1]])) || any(is.na(cv_traindata[[2]])) || any(is.na(cv_testdata))) {
    message(sprintf("Skipping fold %d due to NA values in data.", idx_fold))
    next
  }
  
  # Discriminant Analysis
  tryCatch({
    DA_pred <- discriminantCorpus(cv_traindata, cv_testdata)
    DApredictions <- c(DApredictions, DA_pred)
  }, error = function(e) {
    message(sprintf("Error in DA for fold %d: %s", idx_fold, e$message))
    DApredictions <- c(DApredictions, rep(NA, length(idx)))
  })
  
  # KNN Classification
  tryCatch({
    KNN_pred <- KNNCorpus(cv_traindata, cv_testdata)
    KNNpredictions <- c(KNNpredictions, as.integer(as.character(KNN_pred)))
  }, error = function(e) {
    message(sprintf("Error in KNN for fold %d: %s", idx_fold, e$message))
    KNNpredictions <- c(KNNpredictions, rep(NA, length(idx)))
  })
  
  # Random Forest
  tryCatch({
    RF_pred <- randomForestCorpus(cv_traindata, cv_testdata)
    RFpredictions <- c(RFpredictions, RF_pred)
  }, error = function(e) {
    message(sprintf("Error in RF for fold %d: %s", idx_fold, e$message))
    RFpredictions <- c(RFpredictions, rep(NA, length(idx)))
  })
  
  # True label for this fold's test data
  truth_label_fold <- ifelse(idx <= (num_text / 2), 1, 2)
  truth <- c(truth, truth_label_fold)
}

# 3. Inference and visualize results

# 3.1 Convert numeric to factor
truth <- factor(truth, levels = c(1, 2))
DApredictions <- factor(DApredictions, levels = c(1, 2))
KNNpredictions <- factor(KNNpredictions, levels = c(1, 2))
RFpredictions <- factor(RFpredictions, levels = c(1, 2))

# 3.2 Calculate accuracy
message("Discriminant Analysis (DA) Accuracy: ", sum(DApredictions == truth, na.rm = TRUE) / length(na.omit(DApredictions)))
message("KNN Accuracy: ", sum(KNNpredictions == truth, na.rm = TRUE) / length(na.omit(KNNpredictions)))
message("Random Forest (RF) Accuracy: ", sum(RFpredictions == truth, na.rm = TRUE) / length(na.omit(RFpredictions)))

# 3.3 Print confusion matrix
message("Confusion Matrix for Discriminant Analysis:")
print(confusionMatrix(DApredictions, truth))

message("Confusion Matrix for KNN:")
print(confusionMatrix(KNNpredictions, truth))

message("Confusion Matrix for Random Forest:")
print(confusionMatrix(RFpredictions, truth))






