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
## (hence docFreq should be (ngram + 1) frequency dictionary)
predict <- function (text, docFreq, ngram) {
  text  <- tolower(text)
  tokens <- tokenize(text, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE)[[1]]
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
