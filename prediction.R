library(quanteda)

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
  search <- paste0(word1, "_")
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

## predict the next text using the last "ngram" words in the text
## "tokens" must be a character vector whose length is >= ngram
## (hence docFreq should be (ngram + 1) frequency dictionary)
predict <- function (tokens, docFreq, ngram) {
  if (length(tokens) < ngram) {
    return(character())
  }
  
  tokens <- tokens[(length(tokens) - ngram + 1):length(tokens)]
  search <- paste(tokens, collapse="_")
  search <- paste0(search, "_")
  
  vectnames <- names(docFreq)
  indices <- which(substr(vectnames, 1, nchar(search)) == search)
  result <- sort(docFreq[indices], decreasing = TRUE)
  
  if (length(result) > 0) {
    result <- result / sum(result)
    # result <- result[1:min(3, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}

# gramFreq <- list()
# gramFreq[[2]] <- readRDS("gram2FreqDrop1.rds")
# gramFreq[[3]] <- readRDS("gram3FreqDrop1.rds")
# gramFreq[[4]] <- readRDS("gram4FreqDrop1.rds")
# gramFreq[[5]] <- readRDS("gram5FreqDrop1.rds")
# saveRDS(gramFreq, "gramFreq.rds")

predictNaiveBackup <- function (text) {
  prediction <- character()
  
  text  <- tolower(text)
  tokens <- tokenize(text, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE)[[1]]
  len <- length(tokens)
  if (len < 1) {
    return(prediction)
  }
  
  result <- numeric()
  for (n in min(5, len + 1):2) {
    print(paste("Trying", n, "gram.."))
    result <- predict(tokens, gramFreq[[n]], n - 1)
    if (length(result) > 0) {
      print(paste("Using", n, "gram."))
      prediction <- names(result)[1:min(3, length(result))]
      break;
    }
  }
  return(prediction)
}


## predict the next text using the last "ngram" words in the text
## by removing the stopword and using n-gram.
## (hence docFreq should be (ngram + 1) frequency dictionary)
predictClean <- function (text, docFreq, ngram) {
  text  <- tolower(text)
  tokens <- removeFeatures(tokenize(text, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE), stopwords("english"))[[1]]
  if (length(tokens) < ngram) {
    return()
  }
  
  tokens <- tokens[(length(tokens) - ngram + 1):length(tokens)]
  search <- paste(tokens, collapse="_")
  search <- paste0(search, "_")
  
  vectnames <- names(docFreq)
  indices <- which(substr(vectnames, 1, nchar(search)) == search)
  result <- sort(docFreq[indices], decreasing = TRUE)
  
  if (length(result) > 0) {
    result <- result / sum(result)
    result <- result[1:min(3, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}
