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

# Initialize lists to store overall accuracy and runtime for each model
DA_time_total <- 0
KNN_time_total <- 0
RF_time_total <- 0

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
  
  # Skip the topic if there are not enough files for human or GPT (at least 5 files are needed for LOOCV)
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
  
  # 2.5 Leave-One-Out Cross-Validation (LOOCV)
  for (i in 1:num_text) {
    train_idx <- setdiff(1:num_text, i)  # 所有样本，除了当前的第 i 个
    test_idx <- i                        # 当前样本 i 作为测试集
    
    cv_traindata <- dataset.mat[train_idx, , drop = FALSE]
    cv_testdata <- dataset.mat[test_idx, , drop = FALSE]
    train_labels <- labels[train_idx]
    test_labels <- labels[test_idx]
    
    # Discriminant Analysis
    tryCatch({
      DA_start_time <- proc.time()
      DA_pred <- discriminantCorpus(list(cv_traindata[train_labels == 1, , drop = FALSE], 
                                         cv_traindata[train_labels == 2, , drop = FALSE]), cv_testdata)
      DA_end_time <- proc.time()
      DA_time_total <- DA_time_total + (DA_end_time - DA_start_time)[3]
      DApredictions <- c(DApredictions, DA_pred)
    }, error = function(e) {
      message(sprintf("Discriminant Analysis error, observation %d: %s", i, e$message))
      DApredictions <- c(DApredictions, NA)
    })
    
    # KNN Classification
    tryCatch({
      KNN_start_time <- proc.time()
      KNN_pred <- knn(train = cv_traindata, test = cv_testdata, cl = train_labels, k = 3)
      KNN_end_time <- proc.time()
      KNN_time_total <- KNN_time_total + (KNN_end_time - KNN_start_time)[3]
      KNNpredictions <- c(KNNpredictions, as.integer(as.character(KNN_pred)))
    }, error = function(e) {
      message(sprintf("KNN Classification error, observation %d: %s", i, e$message))
      KNNpredictions <- c(KNNpredictions, NA)
    })
    
    # Random Forest
    tryCatch({
      RF_start_time <- proc.time()
      RF_model <- randomForest(x = cv_traindata, y = as.factor(train_labels))
      RF_pred <- predict(RF_model, cv_testdata)
      RF_end_time <- proc.time()
      RF_time_total <- RF_time_total + (RF_end_time - RF_start_time)[3]
      RFpredictions <- c(RFpredictions, as.integer(as.character(RF_pred)))
    }, error = function(e) {
      message(sprintf("Random Forest error, observation %d: %s", i, e$message))
      RFpredictions <- c(RFpredictions, NA)
    })
  }
}

# Create a dataframe to store the overall runtime for each model
runtime_summary <- data.frame(
  model = c("DA_time", "KNN_time", "RF_time"),
  runtime = c(DA_time_total, KNN_time_total, RF_time_total)
)

# Plot runtime comparison for each model, with values labeled on the bars
runtime_plot <- ggplot(runtime_summary, aes(x = model, y = runtime, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  theme_classic(base_size = 14) +
  labs(title = "Overall Runtime Comparison of Different Models", x = "Model Type", y = "Total Runtime (seconds)", fill = "Model Type") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_text(aes(label = round(runtime, 2)), vjust = -0.5, size = 5)

# Display runtime comparison plot
print(runtime_plot)

# Save runtime comparison plot
ggsave("overall_model_runtime_comparison.png", plot = runtime_plot, width = 12, height = 7, bg = "white")

# 5. Print summary table of runtime
message("\nSummary Table: Overall Model Runtime\n")
print(runtime_summary)

# 6. Save summary table as CSV
write.csv(runtime_summary, file = "overall_runtime_results.csv", row.names = FALSE)
