library(hash)
library(quanteda)
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

predict3 <- function (docFreq4, word1, word2, word3) {
  name4 <- names(docFreq4)
  search <- paste0(word1, "_", word2, "_", word3, "_")
  indices <- which(substr(name4, 1, nchar(search)) == search)
  result <- sort(docFreq4[indices], decreasing = TRUE)
  if (length(result) > 0) {
    result <- result / sum(result)
    result <- result[1:min(3, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}

predict2 <- function (docFreq3, word1, word2) {
  name3 <- names(docFreq3)
  search <- paste0(word1, "_", word2, "_")
  indices <- which(substr(name3, 1, nchar(search)) == search)
  result <- sort(docFreq3[indices], decreasing = TRUE)
  if (length(result) > 0) {
    result <- result / sum(result)
    result <- result[1:min(3, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}

predict1 <- function (docFreq2, word1) {
  name2 <- names(docFreq2)
  search <- paste0(word1, "_", word2, "_")
  indices <- which(substr(name2, 1, nchar(search)) == search)
  result <- sort(docFreq2[indices], decreasing = TRUE)
  if (length(result) > 0) {
    result <- result / sum(result)
    result <- result[1:min(3, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}

# drop all numbers smaller than or equal to the cutoff
dropSmallNumbers <- function (docFreq, cutoff) {
  docFreq[docFreq > cutoff]
}

# blog <- readFileLines("final/en_US/en_US.blogs.txt")
# news <- readFileLines("final/en_US/en_US.news.txt")
# twitter <- readFileLines("final/en_US/en_US.twitter.txt")
# 
# set.seed(1234567)
# 
# trainIndexBlog <- createSamples(length(blog), 0.8)
# trainIndexNews <- createSamples(length(news), 0.8)
# trainIndexTwitter <- createSamples(length(twitter), 0.8)
# 
# trainBlog <- blog[trainIndexBlog]
# testBlog <- blog[-trainIndexBlog]
# trainNews <- news[trainIndexNews]
# testNews <- news[-trainIndexNews]
# trainTwitter <- twitter[trainIndexTwitter]
# testTwitter <- twitter[-trainIndexTwitter]
# 
# trainingData <- shuffleVector(c(trainBlog, trainNews, trainTwitter))
# testData <- shuffleVector(c(testBlog, testNews, testTwitter))
# 
# saveRDS(trainingData, "trainingData.rds")
# saveRDS(testData, "testData.rds")
# 
# 
# trainingData <- readRDS("trainingData.rds")
# 
# testData <- readRDS("testData.rds")
# 
# trainingDFM4 <- dfm(trainingData, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE, language="english", ngrams=4)
# saveRDS(trainingDFM4, "trainingDFM4.rds")
# 
# gram4Freq <- docfreq(trainingDFM4)
# saveRDS(gram4Freq, "gram4Freq.rds")
# 
# gram4FreqDrop1 <- dropSmallNumbers(gram4Freq, 1)
# saveRDS(gram4FreqDrop1, "gram4FreqDrop1.rds")

# trainingDFM3 <- dfm(trainingData, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE, language="english", ngrams=3)
# saveRDS(trainingDFM3, "trainingDFM3.rds")
#
# gram3Freq <- docfreq(trainingDFM3)
# saveRDS(gram3Freq, "gram3Freq.rds")
# 
# gram3FreqDrop1 <- dropSmallNumbers(gram3Freq, 1)
# saveRDS(gram3FreqDrop1, "gram3FreqDrop1.rds")

# trainingDFM2 <- dfm(trainingData, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE, language="english", ngrams=2)
# saveRDS(trainingDFM2, "trainingDFM2.rds")
# 
# gram2Freq <- docfreq(trainingDFM2)
# saveRDS(gram2Freq, "gram2Freq.rds")
# 
# gram2FreqDrop1 <- dropSmallNumbers(gram2Freq, 1)
# saveRDS(gram2FreqDrop1, "gram2FreqDrop1.rds")