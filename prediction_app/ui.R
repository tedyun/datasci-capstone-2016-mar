library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Text Prediction"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h3("Predictions"),
      textOutput("inProgress"),
      textOutput("predWord1"),
      textOutput("predWord2"),
      textOutput("predWord3")
    ),
    mainPanel(
      textInput("predInputText", label = "Input Text", value = ""),
      actionButton("submitButton", "Submit")
    )
  )
))