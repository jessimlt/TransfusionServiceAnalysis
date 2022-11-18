
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(
    tabsetPanel(
      tabPanel("Product by Location", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput("locations", "Transfusion Service Locations", choices = c("", "BB", "C", "CRC"))
                   ),
                 mainPanel(plotlyOutput("g_if"),
                           plotlyOutput("g_is")
                           )
                 )
               ),
      tabPanel("tab2", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5)
                   ),
                 mainPanel(h3('Result of prediction'),
                           h4('You entered '),
                           verbatimTextOutput("inputValue"),
                           h4('Which resulted in a prediction of '),
                           verbatimTextOutput("prediction")
                 )
               )
      )
    )
  )
)

