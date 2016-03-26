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

buildWordDictionary <- function (listTokenized, dictWordToIndex, dictIndexToWord) {
  idx <- 1
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
            dictIndexToWord[[toString(idx)]] <- sWord
            idx <- idx + 1
          }
        }
      }
    }
  }
  idx
}

getTopNWords <- function (wordFreqHash, N) {
  val <- values(wordFreqHash)
  val1 <- sort(val, decreasing=TRUE)
  val1[1:N]
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

wordToIndexDict <- hash()
indexToWordDict <- hash()
buildWordDictionary(newsTokenized, wordToIndexDict, indexToWordDict)
buildWordDictionary(blogTokenized, wordToIndexDict, indexToWordDict)
buildWordDictionary(twitterTokenized, wordToIndexDict, indexToWordDict)
saveRDS(wordToIndexDict, "wordToIndexDict.rds")
saveRDS(indexToWordDict, "indexToWordDict.rds")


