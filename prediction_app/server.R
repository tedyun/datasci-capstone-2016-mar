library(shiny)
library(quanteda)

gramFreq <- readRDS("gramFreq.rds")

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
    nm <- names(result)
    names(result) <- substr(nm, nchar(search) + 1, nchar(nm))
  }
  result
}

predictNaiveBackup <- function (text) {
  print("Prediction function started.")
  prediction <- character()
  
  text  <- tolower(text)
  tokens <- tokenize(text, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = FALSE)[[1]]
  len <- length(tokens)
  if (len < 1) {
    return(prediction)
  }
  
  result <- numeric()
  for (n in min(5, len + 1):2) {
    result <- predict(tokens, gramFreq[[n]], n - 1)
    if (length(result) > 0) {
      prediction <- names(result)[1:min(3, length(result))]
      break;
    }
  }
  print("Prediction function ended.")
  return(prediction)
}

shinyServer(function(input, output) {
  result <- reactive({
    if (input$submitButton > 0) {
      withProgress(message="Predicting next word..", {
        isolate(predictNaiveBackup(input$predInputText))
      })
    }
  })
  
  output$inProgress <- renderText({
    
  })
  
  output$predWord1 <- renderText({ 
    result()[1]
  })
  output$predWord2 <- renderText({ 
    result()[2]
  })
  output$predWord3 <- renderText({ 
    result()[3]
  })
})