library(hash)

## Get the maximum line length
# getMaximumLineLength <- function (dataset) {
#   maxLength <- 0
#   for (line in dataset) {
#     curLength <- nchar(line)
#     if (curLength > maxLength) {
#       maxLength <- curLength
#     }
#   }
#   return(maxLength)
# }

## Tokenize a single line
# tokenizeLine <- function (line) {
#   tokens <- unlist(strsplit(line, "[^a-zA-Z0-9'-]"))
#   tokenFilter <- function (word) {
#     if (
#       (nchar(word) == 0) ||
#       ((nchar(word) == 1) && (tolower(word) != "a"))
#       ) {
#       return(FALSE)
#     }
#     return(TRUE)
#   }
#   return(Filter(tokenFilter, tokens))
# }

## Get the number of lines containing a certain word
## This is too slow...
# getNumberOfLinesContainingWord <- function (dataset, word) {
#   count <- 0
#   for (line in dataset) {
#     tokens <- tokenizeLine(line)
#     if (word %in% tokens) {
#       count <- count + 1
#     }
#   }
#   return(count)
# }

## Find the line numbers contaning a certain string
# getLinesContainingString <- function (dataset, keyword) {
#   lineNumbers <- NULL
#   for (i in 1:length(dataset)) {
#     line <- dataset[i]
#     if (length(grep(keyword, line, fixed=TRUE)) > 0) {
#       lineNumbers <- c(lineNumbers, i)
#     }
#   }
#   return(lineNumbers)
# }

# loveLines <- getLinesContainingString(dataset_twitter, 'love')
# hateLines <- getLinesContainingString(dataset_twitter, 'hate')
# length(loveLines) / length(hateLines)
# biostatLines <- getLinesContainingString(dataset_twitter, 'biostats')
# kickboxing <- getLinesContainingString(dataset_twitter, "A computer once beat me at chess, but it was no match for me at kickboxing")


readFileLines <- function (sFilename) {
  conn = file(sFilename, open = "r")
  result <- readLines(conn)
  close(conn)
  return(result)
}

getMaxLineLength <- function (sVec) {
  len <- length(sVec)
  max <- 0
  for (i in 1:len) {
    lineLen <- nchar(sVec[i])
    if (max < lineLen) {
      max <- lineLen
    }
  }
  return(max)
}

getNumberOfLinesWithSubstring <- function (sVec, sSub) {
  len <- length(sVec)
  num <- 0
  for (i in 1:len) {
    if (length(grep(sSub, sVec[i])) > 0) {
      num <- num + 1
    }
  }
  return(num)
}

findLinesWithSubstring <- function (sVec, sSub) {
  len <- length(sVec)
  vec <- vector(mode="integer")
  for (i in 1:len) {
    if (length(grep(sSub, sVec[i])) > 0) {
      vec <- append(vec, i)
    }
  }
  return(vec)
}


#### TOKENIZATION ####

## Break a line into sublines, each separated by punctuation (.!?), quotation ("'), colons (:;), or dash ( - , -- )
subLineRegex = "( - )|( -- )|([.!?\":;]+[ ]*[.!?\":;]*)"
splitRegex = "[-,\t\n\r ]+"
splitIntoSubLines <- function (sLine) {
  unlist(strsplit(sLine, subLineRegex, fixed = FALSE))
}
tokenizeSubline <- function (sSubLine) {
  unlist(strsplit(sSubLine, splitRegex, fixed = FALSE))
}
tokenizeLine <- function (sLine) {
  tokList <- list()
  subLines <- splitIntoSubLines(sLine)
  for (i in 1:length(subLines)) {
    tokList[[i]] <- tokenizeSubline(subLines[i])
  }
  tokList
}
getNumberOfWordsInLine <- function (sLine) {
  tokenized <- tokenizeLine(sLine)
  len <- 0
  for (i in 1:length(tokenized)){
    subLine <- tokenized[[i]]
    len <- len + length(subLine)
  }
  len
}

countWordFrequency <- function (listTokenized) {
  dict <- hash()
  for (i in 1:length(listTokenized)) {
    if (i %% 10000 == 0) {
      print (paste(i, "-th line processed."))
    }
    sublist <- listTokenized[[i]]
    for (j in 1:length(sublist)) {
      sVec <- sublist[[j]]
      for (k in 1:length(sVec)){
        sWord <- sVec[k]
        if (!is.na(sWord) && length(sWord) > 0 && nchar(sWord) > 0) {
          sWord <- tolower(sWord)
          if (length(sWord) > 0 && nchar(sWord) > 0) {
            dict[[sWord]] <- if (is.null(dict[[sWord]])) 1 else dict[[sWord]] + 1
          }
        }
      }
    }
  }
  dict
}

buildWordDictionary <- function (listTokenized, dictWordToIndex, vectIndexToWord) {
  idx <- length(vectIndexToWord) + 1
  for (i in 1:length(listTokenized)) {
    if (i %% 10000 == 0) {
      print (paste(i, "-th line processed."))
    }
    sublist <- listTokenized[[i]]
    for (j in 1:length(sublist)) {
      sVec <- sublist[[j]]
      for (k in 1:length(sVec)){
        sWord <- sVec[k]
        if (!is.na(sWord) && length(sWord) > 0 && nchar(sWord) > 0) {
          sWord <- tolower(sWord)
          if (is.null(dictWordToIndex[[sWord]])) {
            dictWordToIndex[[sWord]] <- idx
            vectIndexToWord[idx] <- sWord
            idx <- idx + 1
          }
        }
      }
    }
  }
  vectIndexToWord
}

getTopNWords <- function (wordFreqHash, N) {
  val <- values(wordFreqHash)
  val1 <- sort(val, decreasing=TRUE)
  val1[1:N]
}


growRow <- function (mat) {
  rowN <- nrow(mat)
  colN <- ncol(mat)
  mat2 <- matrix(NA, nrow=rowN, ncol=colN)
  return(rbind(mat, mat2))
}

isString <- function (sWord) {
  return(!is.na(sWord) && length(sWord) > 0 && nchar(sWord) > 0)
}

findRow3 <- function (mat, row, curRow) {
  found <- FALSE
  limit <- min(curRow, nrow(mat))
  for (idx in 1:limit) {
    if (identical(mat[idx,][1:3], row)) {
      found <- TRUE
      break
    }
  }
  if (found) {
    return(idx)
  } else {
    return(-1)
  }
}

# 4*N matrix
trainMatrix <- function (mat, listTokenized, wordToIndexDict) {
  # find the current row
  curRow <- 1
  if (nrow(mat) > 0) {
    for (curRow in 1:nrow(mat)) {
      if (is.na(mat[curRow, 1])) {
        break
      }
    }
  }
  print(paste("Starting at", curRow, "th row"))
  print("Building index..")
  hashIndex <- hash()
  if (curRow > 1) {
    for (i in 1:(curRow - 1)) {
      key <- paste(mat[i, 1], mat[i, 2], mat[i, 3])
      hashIndex[[key]] <- i
    }
  }
  print("Index built.")
  
  
  for (i in 1:length(listTokenized)) {
    if (i %% 1000 == 0) {
      print (paste(i, "-th line trained."))
    }
    sublist <- listTokenized[[i]]
    for (j in 1:length(sublist)) {
      sVec <- sublist[[j]]
      if (length(sVec) < 3) {
        next
      }
      for (k in 1:(length(sVec) - 2)){
        sWord1 <- sVec[k]
        sWord2 <- sVec[k + 1]
        sWord3 <- sVec[k + 2]
        if (isString(sWord1) && isString(sWord2) && isString(sWord3)) {
          n1 <- wordToIndexDict[[tolower(sWord1)]]
          n2 <- wordToIndexDict[[tolower(sWord2)]]
          n3 <- wordToIndexDict[[tolower(sWord3)]]
          ## print(paste(sWord1, sWord2, sWord3))
          if (!is.null(n1) && !is.null(n2) && !is.null(n3)) {
            ## find the row
#             found <- FALSE
#             for (idx in 1:nrow(mat)) {
#               if (identical(mat[idx,][1:3], row)) {
#                 found <- TRUE
#                 break
#               }
#             }
#             if (!found) {
#               idx <- -1
#             }
            ## idx <- findRow3(mat, c(n1, n2, n3), curRow)
            ## idx <- which(mat[,1]==n1 & mat[,2] == n2 & mat[,3] == n3, arr.ind = TRUE)
            hashkey <- paste(n1, n2, n3)
            idx <- hashIndex[[hashkey]]
            if (!is.null(idx)) {
              mat[idx, 4] <- mat[idx, 4] + 1
            } else {
              if (curRow > nrow(mat)){
                print("Growing matrix..")
                rowN <- nrow(mat)
                colN <- ncol(mat)
                mat <- rbind(mat, matrix(NA, nrow=rowN, ncol=colN))
                print(paste("Matrix grown.", nrow(mat)))
              }
              # Growing 1 row
              mat[curRow,] <- c(n1, n2, n3, 1)
              hashIndex[[hashkey]] <- curRow
              curRow <- curRow + 1
            }
          }
        }
      }
    }
  }
  mat
}

combineMatrices <- function (mat1, mat2) {
  mat <- rbind(mat1, matrix(NA, nrow = nrow(mat2), ncol = ncol(mat2)))
  
  # find the current row
  curRow <- 1
  if (nrow(mat) > 0) {
    for (curRow in 1:nrow(mat)) {
      if (is.na(mat[curRow, 1])) {
        break
      }
    }
  }
  
  print("Building index..")
  hashIndex <- hash()
  if (curRow > 1) {
    for (i in 1:(curRow - 1)) {
      key <- paste(mat[i, 1], mat[i, 2], mat[i, 3])
      hashIndex[[key]] <- i
    }
  }
  print("Index built.")
  
  for (i in 1:nrow(mat2)) {
    if (is.na(mat2[i, 1])) {
      break
    }
    if (i %% 100000 == 0) {
      print (paste(i, "-th line combined."))
    }
    n1 <- mat2[i, 1]
    n2 <- mat2[i, 2]
    n3 <- mat2[i, 3]
    n4 <- mat2[i, 4]
    hashkey <- paste(n1, n2, n3)
    idx <- hashIndex[[hashkey]]
    if (!is.null(idx)) {
      mat[idx, 4] <- mat[idx, 4] + n4
    } else {
      # Growing 1 row
      mat[curRow,] <- c(n1, n2, n3, n4)
      hashIndex[[hashkey]] <- curRow
      curRow <- curRow + 1
    }
  }
  mat
}

predict <- function (mat, sWord1, sWord2, wordToIndexDict, indexToWordVect) {
  sub <- mat[which(mat[,1] == wordToIndexDict[[sWord1]] & mat[,2] == wordToIndexDict[[sWord2]], arr.ind=TRUE),]
  sub <- data.frame(sub[,3:4])
  sub <- sub[order(-sub[2]),]
  sub[1] <- sapply(sub[1], function (idx) { indexToWordVect[idx] }, USE.NAMES = FALSE)
  return(sub)
}

blogs <- readFileLines("final/en_US/en_US.blogs.txt")
news <- readFileLines("final/en_US/en_US.news.txt")
twitter <- readFileLines("final/en_US/en_US.twitter.txt")

blogLines <- nchar(blogs)
newsLines <- nchar(news)
twitterLines <- nchar(twitter)

newsWords <- sapply(news, getNumberOfWordsInLine, simplify=TRUE, USE.NAMES = FALSE)
blogWords <- sapply(blogs, getNumberOfWordsInLine, simplify=TRUE, USE.NAMES = FALSE)
twitterWords <- sapply(twitter, getNumberOfWordsInLine, simplify=TRUE, USE.NAMES = FALSE)

newsTokenized <- lapply(news, tokenizeLine)
blogTokenized <- lapply(blogs, tokenizeLine)
twitterTokenized <- lapply(twitter, tokenizeLine)

newsWordFreqHash <- countWordFrequency(newsTokenized)
blogWordFreqHash <- countWordFrequency(blogTokenized)
twitterWordFreqHash <- countWordFrequency(twitterTokenized)

newsTop50 <- getTopNWords(newsWordFreqHash, 50)

# saveRDS(blogTokenized, "blogTokenized.rds")
# saveRDS(newsTokenized, "newsTokenized.rds")
# saveRDS(twitterTokenized, "twitterTokenized.rds")

# saveRDS(blogLines, "blogLines.rds")
# saveRDS(newsLines, "newsLines.rds")
# saveRDS(twitterLines, "twitterLines.rds")

saveRDS(newsWordFreqHash, "newsWordFreqHash.rds")

# getMaxLineLength(blogs)
# getMaxLineLength(news)
# getMaxLineLength(twitter)
# 
# getNumberOfLinesWithSubstring(twitter, "love")
# getNumberOfLinesWithSubstring(twitter, "hate")
# 
# findLinesWithSubstring(twitter, "biostats")

newsTokenized <- readRDS("newsTokenized.rds")
blogTokenized <- readRDS("blogTokenized.rds")
twitterTokenized <- readRDS("twitterTokenized.rds")

wordToIndexDict <- hash()
indexToWordVect <- character()
indexToWordVect <- buildWordDictionary(newsTokenized, wordToIndexDict, indexToWordVect)
indexToWordVect <- buildWordDictionary(blogTokenized, wordToIndexDict, indexToWordVect)
indexToWordVect <- buildWordDictionary(twitterTokenized, wordToIndexDict, indexToWordVect)
saveRDS(wordToIndexDict, "wordToIndexDict.rds")
saveRDS(indexToWordVect, "indexToWordVect.rds")

wordToIndexDict <- readRDS("wordToIndexDict.rds")
indexToWordVect <- readRDS("indexToWordVect.rds")

matTrained <- matrix(NA, nrow = 1000, ncol = 4)
matTrained <- trainMatrix(matTrained, newsTokenized[1:1000], wordToIndexDict)
saveRDS(matTrained, "matTrained_news1000.rds")

matTrained <- trainMatrix(matTrained, newsTokenized[1001:2000], wordToIndexDict)
saveRDS(matTrained, "matTrained_news2000.rds")

#reading necessary components
newsTokenized <- readRDS("newsTokenized.rds")
blogTokenized <- readRDS("blogTokenized.rds")
twitterTokenized <- readRDS("twitterTokenized.rds")
wordToIndexDict <- readRDS("wordToIndexDict.rds")
indexToWordVect <- readRDS("indexToWordVect.rds")

matTrained <- readRDS("matTrained_news2000.rds")

matTrained <- trainMatrix(matTrained, newsTokenized[2001:3000], wordToIndexDict)
saveRDS(matTrained, "matTrained_news3000.rds")

matTrained <- trainMatrix(matTrained, newsTokenized[3001:10000], wordToIndexDict)
saveRDS(matTrained, "matTrained_news10000.rds")

matTrained <- trainMatrix(matTrained, newsTokenized[10001:20000], wordToIndexDict)
saveRDS(matTrained, "matTrained_news20000.rds")

#blog
blogmatTrained <- matrix(NA, nrow = 1000, ncol = 4)
blogmatTrained <- trainMatrix(blogmatTrained, blogTokenized[0:10000], wordToIndexDict)
saveRDS(blogmatTrained, "matTrained_blog10000.rds")

