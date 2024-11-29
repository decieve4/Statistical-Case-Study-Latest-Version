source("./SEM1/Project2/stylometryfunctions.R")

# Load necessary packages
library(ggplot2)
library(reshape2)
library(class)
library(randomForest)
library(caret)
library(pheatmap)
library(tidyr)

# Set seed for reproducibility
set.seed(42)

# 1. Get the list of topics
topics <- list.files("SEM1/Project2/functionwords/functionwords/humanfunctionwords/")
num_topics <- length(topics)

# Initialize lists to store overall accuracy and runtime for each model
DA_time_total <- 0
KNN_time_total <- 0
RF_time_total <- 0
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
  
  # Initialize prediction lists and track runtime
  DApredictions <- NULL
  KNNpredictions <- NULL
  RFpredictions <- NULL
  
  # 2.4 Leave-One-Out Cross-Validation (LOOCV)
  for (i in 1:num_text) {
    train_idx <- setdiff(1:num_text, i)
    test_idx <- i
    
    cv_traindata <- dataset.mat[train_idx, , drop = FALSE]
    cv_testdata <- dataset.mat[test_idx, , drop = FALSE]
    train_labels <- labels[train_idx]
    
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
  
  # 3. Calculate accuracy for each model
  truth <- factor(labels, levels = c(1, 2))
  DApredictions <- factor(DApredictions, levels = c(1, 2))
  KNNpredictions <- factor(KNNpredictions, levels = c(1, 2))
  RFpredictions <- factor(RFpredictions, levels = c(1, 2))
  
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

# Summarize accuracy results for all topics
results_df <- do.call(rbind, results_list)
colnames(results_df) <- c("Dataset", "DA", "KNN", "RF")

# Convert data to long format for ggplot2
results_long <- pivot_longer(results_df, 
                             cols = c("DA", "KNN", "RF"), 
                             names_to = "Model", 
                             values_to = "Accuracy")

# Load baseline result for reference (corrected file path)
baseline_results <- read.csv("./SEM1/Project2/Experiment_Result/Baseline_Result.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(baseline_results) <- c("Dataset", "DA", "KNN", "RF")
baseline_long <- pivot_longer(baseline_results, 
                              cols = c("DA", "KNN", "RF"), 
                              names_to = "Model", 
                              values_to = "Accuracy")

# Plot violin plot comparing accuracy of different models for each topic with baseline lines
accuracy_plot <- ggplot(results_long, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_violin(trim = TRUE, alpha = 0.5) +  # 使用图二的风格，透明度设置为0.5，去除带宽调整
  geom_jitter(width = 0.2, alpha = 0.5) +  # 保持图二的点抖动参数
  geom_hline(data = baseline_long, aes(yintercept = Accuracy, color = Model), linetype = "dashed", size = 1) +  # 保留基准线
  scale_fill_brewer(palette = "Set2") +  # 保持与基准线一致的颜色
  scale_color_brewer(palette = "Set2") +
  theme_bw() +  # 使用 theme_bw() 样式，带有白色背景和网格线
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold")  # 标题居中和加粗
  )

# Display violin plot
print(accuracy_plot)

# Save violin plot using ggsave() and ensure the consistent output format
ggsave("./SEM1/Project2/Figures/Task2_accuracy_distribution_modified.pdf", 
       plot = accuracy_plot, 
       width = 8, 
       height = 6)

# Create a dataframe to store the overall runtime for each model
runtime_summary <- data.frame(
  model = c("DA_time", "KNN_time", "RF_time"),
  runtime = c(DA_time_total, KNN_time_total, RF_time_total)
)

# Plot runtime comparison for each model, with values labeled on the bars
runtime_plot <- ggplot(runtime_summary, aes(x = model, y = runtime, fill = model)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) +
  labs(title = "Overall Runtime Comparison of Different Models", x = "Model Type", y = "Total Runtime (seconds)", fill = "Model Type") +
  geom_text(aes(label = round(runtime, 2)), vjust = -0.5, size = 5)

# Display runtime comparison plot
print(runtime_plot)

# Save runtime comparison plot using theme_bw()
ggsave("./SEM1/Project2/Figures/Task2_runtime_comparison.pdf", plot = runtime_plot, width = 12, height = 7, bg = "white")

# Print summary table of accuracy
message("\nSummary Table: Model Accuracy for Each Topic\n")
print(results_df)

# Save summary table as CSV
write.csv(results_df, file = "./SEM1/Project2/Experiment_Result/Task2_summary_accuracy_results.csv", row.names = FALSE)

# Print summary table of runtime
message("\nSummary Table: Overall Model Runtime\n")
print(runtime_summary)

# Save summary table as CSV
write.csv(runtime_summary, file = "./SEM1/Project2/Experiment_Result/Task2_overall_runtime_results.csv", row.names = FALSE)