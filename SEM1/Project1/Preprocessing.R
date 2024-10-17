source("SEM1/Project1/stylometryfunctions.R")

M <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

features <- M$features
authornames <- M$authornames

X <- do.call(rbind, M$features)
rownames(X) <- unlist(M$booknames)

# Define the 70-dimensional column names (word list)
colnames_70 <- c("a", "all", "also", "an", "and", "any", "are", "as", "at", 
                 "be", "been", "but", "by", "can", "do", "down", "even", "every", 
                 "for", "from", "had", "has", "have", "her", "his", "if", "in", 
                 "into", "is", "it", "its", "may", "more", "must", "my", "no", 
                 "not", "now", "of", "on", "one", "only", "or", "our", "shall", 
                 "should", "so", "some", "such", "than", "that", "the", "their", 
                 "then", "there", "things", "this", "to", "up", "upon", "was", 
                 "were", "what", "when", "which", "who", "will", "with", "would", 
                 "your")

# Assign the column names to the data matrix X
colnames(X) <- colnames_70

sum(is.na(X))

# (Stop Words Removal)

stop_words <- c("the", "and", "of", "in", "to", "a", "is", "it", "with", "that")
X <- X[, !colnames(X) %in% stop_words]

# (Low Freq Words Removal)

low_freq_threshold <- 3
X <- X[, colSums(X > 0) >= low_freq_threshold]

# (Normalization)

row_sums <- rowSums(X)
X <- sweep(X, 1, row_sums, "/")

# (Sparse Features Removal)

sparse_threshold <- 0.9
X <- X[, colMeans(X == 0) < sparse_threshold]

# (Standardization)

X <- scale(X)

# PCA

# Perform PCA on the normalized and standardized data
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)

# Check the proportion of variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2,]

# Plot a scree plot to visualize the variance explained by each component
plot(explained_variance, type = "b", xlab = "Principal Components", ylab = "Proportion of Variance Explained", main = "Scree Plot")

# Calculate the cumulative variance and choose the number of components that explain at least 80% of the variance
cumulative_variance <- cumsum(explained_variance)
num_components <- which(cumulative_variance >= 0.9)[1]

# Extract the principal components that explain at least 80% of the variance
X_pca <- pca_result$x[, 1:num_components]

