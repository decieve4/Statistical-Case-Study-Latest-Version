source("SEM1/Project2/reducewords.R")
source("SEM1/Project2/stylometryfunctions.R")

# 1. setting hyper
numwords <- 500 #number of words to trim the test set down into
topic <- 4 #Architecture

# 2. load data
humanM <- loadCorpus("SEM1/Project2/functionwords/functionwords/humanfunctionwords/", "functionwords")
GPTM <- loadCorpus("SEM1/Project2/functionwords/functionwords/GPTfunctionwords/", "functionwords")
# 2.1 extract specific topic
humanfeatures <- humanM$features[[topic]] #select the essays on this particular topic
GPTfeatures <- GPTM$features[[topic]]

# 3. combine Human and GPT
features <- rbind(humanfeatures, GPTfeatures) #this is a matrix of both human and GPT essays
#here i use author=0 for human, and author=1 for ChatGPT
authornames <- c(rep(0,nrow(humanfeatures)), rep(1,nrow(GPTfeatures)))

# rownames(features) <- as.character(authornames)

# 4. reduce process
#now reduce the essays down to numwords words
reducedhumanfeatures <- reducewords(humanfeatures, numwords)
reducedGPTfeatures <- reducewords(GPTfeatures, numwords)
reducedfeatures <- rbind(reducedhumanfeatures, reducedGPTfeatures)

# 5. Run Classifier
for (i in 1:nrow(features)) {
  train <- features[-i,]
  test <- reducedfeatures[i,,drop=FALSE] #note that only the test set size changes
  myKNN(train, test, authornames, k=1)
}
