
library(shiny)
library(plotly)

##define UI
shinyUI(
  
  fluidPage(
    tabsetPanel(
      tabPanel("Product by Location", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput("locations", 
                               "Transfusion Service Locations", 
                               choices = c("BB", "C", "CRC"),
                               selected = "BB")
                   ),
                 mainPanel(plotlyOutput("g_if"),
                           plotlyOutput("g_is"),
                           plotlyOutput("g_ifvis")
                           )
                 )
               ),
      tabPanel("Antibody Screens by Order", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput("ordercodes", 
                               "Order Types", 
                               choices = c("Crossmatch", "Type and Screen", "Other %AS battery"),
                               selected = "Crossmatch")
                   ),
                 mainPanel(plotlyOutput("g_t_counts"),
                           plotlyOutput("g_t_method"),
                           plotlyOutput("g_t_results"),
                           plotlyOutput("g_t_priority")
                 )
               )
      )
    )
  )
)

