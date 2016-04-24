library(shiny)
library(quanteda)

gramFreqLight <- readRDS("gramFreqLight.rds")

predict <- function (tokens, docFreq, ngram, maxSuggest, excludeWords) {
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
    cutLength <- maxSuggest + length(excludeWords)
    result <- result[1:min(cutLength, length(result))]
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
    # exclude words
    if (length(excludeWords) > 0) {
      result <- result[!(names(result) %in% excludeWords)]
    }
  }
  result <- result[1:min(maxSuggest, length(result))]
  result
}

# predict using "Stupid Backoff" model
alpha <- 0.4
predictStupidBackoff <- function (text) {
  print(paste0("Start prediction for '", text, "'"))
  prediction <- character()
  
  text  <- tolower(text)
  tokens <- tokenize(text, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE)[[1]]
  len <- length(tokens)
  if (len < 1) {
    return(prediction)
  }
  
  result <- numeric()
  counter <- 0
  excludeWords <- character()
  for (n in min(5, len + 1):2) {
    counter <- counter + 1
    maxSuggest <- counter * 3
    result <- c(result, alpha^(counter - 1) * predict(tokens, gramFreqLight[[n]], n - 1, maxSuggest, excludeWords))
    excludeWords <- c(excludeWords, names(result))
  }
  result <- sort(result, decreasing = TRUE)
  result <- result[1:min(3, length(result))]
  print(result)
  if (is.na(result) || !length(result)) {
    prediction <- character()
  } else {
    prediction <- names(result)
  }
  prediction <- c(prediction, "the", "to", "a", "and", "i", "of")
  prediction <- unique(prediction)
  prediction <- prediction[1:3]
  print(paste0("Finished prediction for '", text, "'"))
  return(prediction)
}

shinyServer(function(input, output) {
  result <- reactive({
    if (input$submitButton > 0) {
      withProgress(message="Predicting next word..", {
        isolate(predictStupidBackoff(input$predInputText))
      })
    }
  })
  
#   output$inProgress <- renderText({
#     "Click submit button to show predicted words."
#   })
  
  output$predWord1 <- renderText({ 
    paste0("choice 1: <strong>", result()[1], "</strong>")
  })
  output$predWord2 <- renderText({ 
    paste0("choice 2: <strong>", result()[2], "</strong>")
  })
  output$predWord3 <- renderText({ 
    paste0("choice 3: <strong>", result()[3], "</strong>")
  })
})