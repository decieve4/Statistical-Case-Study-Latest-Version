# Load necessary packages
library(ggplot2)
library(reshape2)
library(class)
library(randomForest)
library(caret)
library(pheatmap)

# Set seed for reproducibility
set.seed(42)

# 1. Get the list of topics
topics <- list.files("SEM1/Project2/functionwords/functionwords/humanfunctionwords/")
num_topics <- length(topics)

# Initialize list to store accuracy results for each topic
results_list <- list()

# 2. Loop through each topic to perform classification analysis
for (current_topic in 1:num_topics) {
  topic_name <- topics[current_topic]
  message(sprintf("Processing topic %s...", topic_name))
  
  # 2.1 Get human and GPT function word frequencies for the current topic
  human_topic_dir <- file.path("SEM1/Project2/functionwords/functionwords/humanfunctionwords", topic_name)
  GPT_topic_dir <- file.path("SEM1/Project2/functionwords/functionwords/GPTfunctionwords", topic_name)
  
  # Read all .txt files for the current topic
  human_files <- list.files(human_topic_dir, full.names = TRUE)
  GPT_files <- list.files(GPT_topic_dir, full.names = TRUE)
  
  # Skip the topic if there are not enough files for human or GPT (at least 5 files are needed for 5-fold CV)
  if (length(human_files) < 5 || length(GPT_files) < 5) {
    message(sprintf("Skipping topic %s due to insufficient data", topic_name))
    next
  }
  
  # 2.2 Read function word frequencies into matrices
  human_topic_features <- do.call(rbind, lapply(human_files, function(file) {
    as.numeric(unlist(strsplit(readLines(file), split = ",")))
  }))
  
  GPT_topic_features <- do.call(rbind, lapply(GPT_files, function(file) {
    as.numeric(unlist(strsplit(readLines(file), split = ",")))
  }))
  
  # 2.3 Combine human and GPT data
  dataset.mat <- rbind(human_topic_features, GPT_topic_features)
  labels <- c(rep(1, nrow(human_topic_features)), rep(2, nrow(GPT_topic_features)))
  
  # Fill missing values with column means
  dataset.mat[is.na(dataset.mat)] <- apply(dataset.mat, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  
  num_text <- nrow(dataset.mat)
  
  # 2.4 Initialize prediction lists
  DApredictions <- NULL
  KNNpredictions <- NULL
  RFpredictions <- NULL
  
  # 2.5 5-Fold Cross-Validation (replace Leave-One-Out to improve efficiency)
  folds <- createFolds(labels, k = 5)
  for (fold_idx in 1:5) {
    train_idx <- unlist(folds[-fold_idx])
    test_idx <- unlist(folds[fold_idx])
    
    cv_traindata <- dataset.mat[train_idx, , drop = FALSE]
    cv_testdata <- dataset.mat[test_idx, , drop = FALSE]
    train_labels <- labels[train_idx]
    test_labels <- labels[test_idx]
    
    # Discriminant Analysis
    tryCatch({
      DA_pred <- discriminantCorpus(list(cv_traindata[train_labels == 1, , drop = FALSE], 
                                         cv_traindata[train_labels == 2, , drop = FALSE]), cv_testdata)
      DApredictions <- c(DApredictions, DA_pred)
    }, error = function(e) {
      message(sprintf("Discriminant Analysis error, fold %d: %s", fold_idx, e$message))
      DApredictions <- c(DApredictions, rep(NA, length(test_labels)))
    })
    
    # KNN Classification
    tryCatch({
      KNN_pred <- knn(train = cv_traindata, test = cv_testdata, cl = train_labels, k = 3)
      KNNpredictions <- c(KNNpredictions, as.integer(as.character(KNN_pred)))
    }, error = function(e) {
      message(sprintf("KNN Classification error, fold %d: %s", fold_idx, e$message))
      KNNpredictions <- c(KNNpredictions, rep(NA, length(test_labels)))
    })
    
    # Random Forest
    tryCatch({
      RF_model <- randomForest(x = cv_traindata, y = as.factor(train_labels))
      RF_pred <- predict(RF_model, cv_testdata)
      RFpredictions <- c(RFpredictions, as.integer(as.character(RF_pred)))
    }, error = function(e) {
      message(sprintf("Random Forest error, fold %d: %s", fold_idx, e$message))
      RFpredictions <- c(RFpredictions, rep(NA, length(test_labels)))
    })
  }
  
  # 3. Inference and visualization
  truth <- factor(labels, levels = c(1, 2))
  DApredictions <- factor(DApredictions, levels = c(1, 2))
  KNNpredictions <- factor(KNNpredictions, levels = c(1, 2))
  RFpredictions <- factor(RFpredictions, levels = c(1, 2))
  
  # 3.1 Calculate accuracy
  DA_accuracy <- sum(DApredictions == truth, na.rm = TRUE) / length(na.omit(DApredictions))
  KNN_accuracy <- sum(KNNpredictions == truth, na.rm = TRUE) / length(na.omit(KNNpredictions))
  RF_accuracy <- sum(RFpredictions == truth, na.rm = TRUE) / length(na.omit(RFpredictions))
  
  # Save accuracy results for the current topic
  results_list[[current_topic]] <- data.frame(
    topic = topic_name,
    DA_accuracy = DA_accuracy,
    KNN_accuracy = KNN_accuracy,
    RF_accuracy = RF_accuracy
  )
}

# 4. Summarize accuracy results for all topics
results_df <- do.call(rbind, results_list)

# Convert data to long format for ggplot2
results_long <- reshape2::melt(results_df, id.vars = "topic", variable.name = "model", value.name = "accuracy")

# Plot bar chart comparing accuracy of different models for each topic
accuracy_plot <- ggplot(results_long, aes(x = reorder(topic, -accuracy), y = accuracy, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  theme_classic(base_size = 14) +
  labs(title = "Accuracy Comparison of Different Models for Each Topic", x = "Topic", y = "Accuracy", fill = "Model Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_brewer(palette = "Set2")

# Display bar chart
print(accuracy_plot)

# Save bar chart
ggsave("model_accuracy_comparison.png", plot = accuracy_plot, width = 12, height = 7, bg = "white")

# 5. Print summary table
message("\nSummary Table: Model Accuracy for Each Topic\n")
print(results_df)

# 6. Save summary table as CSV
write.csv(results_df, file = "summary_accuracy_results.csv", row.names = FALSE)
