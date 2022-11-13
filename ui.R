
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  pageWithSidebar(
    headerPanel("Transfusion Service Analysis"),
    
    sidebarPanel(
      selectInput("locations", "Transfusion Locations", choices = c("", "BB", "C", "CRC")),
      numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5)
    ),
    
    mainPanel(
      plotOutput("g_if"),
      h3('Result of prediction'),
      h4('You entered '),
      verbatimTextOutput("inputValue"),
      h4('Which resulted in a prediction of '),
      verbatimTextOutput("prediction")
    )

  
  )
)
