library(hash)
library(quanteda)
library(caret)
library(permute)

readFileLines <- function (sFilename) {
  conn = file(sFilename, open = "r")
  result <- readLines(conn)
  close(conn)
  return(result)
}

createSamples <- function (totalNum, ratio){
  sort(sample(totalNum, size = totalNum * ratio))
}

shuffleVector <- function (vector){
  vector[shuffle(length(vector))]
}

predict3 <- function (docFreq, word1, word2, word3) {
  name4 <- names(docFreq)
  search <- paste0(word1, "_", word2, "_", word3, "_")
  indices <- which(substr(name4, 1, nchar(search)) == search)
  result <- sort(docFreq[indices], decreasing = TRUE)
  result <- result / sum(result)
  result
}

blog <- readFileLines("final/en_US/en_US.blogs.txt")
news <- readFileLines("final/en_US/en_US.news.txt")
twitter <- readFileLines("final/en_US/en_US.twitter.txt")

set.seed(1234567)

trainIndexBlog <- createSamples(length(blog), 0.8)
trainIndexNews <- createSamples(length(news), 0.8)
trainIndexTwitter <- createSamples(length(twitter), 0.8)

trainBlog <- blog[trainIndexBlog]
testBlog <- blog[-trainIndexBlog]
trainNews <- news[trainIndexNews]
testNews <- news[-trainIndexNews]
trainTwitter <- twitter[trainIndexTwitter]
testTwitter <- twitter[-trainIndexTwitter]

trainingData <- shuffleVector(c(trainBlog, trainNews, trainTwitter))
testData <- shuffleVector(c(testBlog, testNews, testTwitter))

saveRDS(trainingData, "trainingData.rds")
saveRDS(testData, "testData.rds")

# 
# trainingData <- readRDS("trainingData.rds")
# testData <- readRDS("testData.rds")
# 

trainingDFM4 <- dfm(trainingData[1:500000], toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE, language="english", ngrams=4)

gram4Freq <- docfreq(trainingDFM4)