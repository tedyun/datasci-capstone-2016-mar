library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Text Prediction"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h3("Predictions"),
      htmlOutput("predWord1"),
      htmlOutput("predWord2"),
      htmlOutput("predWord3")
    ),
    mainPanel(
      helpText("Click the Submit button to predict the next word."),
      textInput("predInputText", label = "Input Sentence", value = ""),
      actionButton("submitButton", "Submit")
    )
  )
))