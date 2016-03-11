con <- file("final/en_US/en_US.twitter.txt", "r")
dataset_twitter <- readLines(con)
close(con)

con <- file("final/en_US/en_US.news.txt", "r")
dataset_news <- readLines(con)
close(con)

con <- file("final/en_US/en_US.blogs.txt", "r")
dataset_blogs <- readLines(con)
close(con)

## Get the maximum line length
getMaximumLineLength <- function (dataset) {
  maxLength <- 0
  for (line in dataset) {
    curLength <- nchar(line)
    if (curLength > maxLength) {
      maxLength <- curLength
    }
  }
  return(maxLength)
}

## Tokenize a single line
tokenizeLine <- function (line) {
  tokens <- unlist(strsplit(line, "[^a-zA-Z0-9'-]"))
  tokenFilter <- function (word) {
    if (
      (nchar(word) == 0) ||
      ((nchar(word) == 1) && (tolower(word) != "a"))
      ) {
      return(FALSE)
    }
    return(TRUE)
  }
  return(Filter(tokenFilter, tokens))
}

## Get the number of lines containing a certain word
## This is too slow...
getNumberOfLinesContainingWord <- function (dataset, word) {
  count <- 0
  for (line in dataset) {
    tokens <- tokenizeLine(line)
    if (word %in% tokens) {
      count <- count + 1
    }
  }
  return(count)
}

## Find the line numbers contaning a certain string
getLinesContainingString <- function (dataset, keyword) {
  lineNumbers <- NULL
  for (i in 1:length(dataset)) {
    line <- dataset[i]
    if (length(grep(keyword, line, fixed=TRUE)) > 0) {
      lineNumbers <- c(lineNumbers, i)
    }
  }
  return(lineNumbers)
}

# loveLines <- getLinesContainingString(dataset_twitter, 'love')
# hateLines <- getLinesContainingString(dataset_twitter, 'hate')
# length(loveLines) / length(hateLines)
# biostatLines <- getLinesContainingString(dataset_twitter, 'biostats')
# kickboxing <- getLinesContainingString(dataset_twitter, "A computer once beat me at chess, but it was no match for me at kickboxing")