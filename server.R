
library(shiny)
library(tidyverse)
library(ggplot2)

##most recent year
file_id <- as.numeric(format(Sys.Date(), '%Y'))-1

##read data
##query in tsa_query
##data prep in tsa_shiny_data_prep
bb_if_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_if_data_fy", file_id, ".rds"))
bb_is_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_is_data_fy", file_id, ".rds"))
bb_t_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_t_data_fy", file_id, ".rds"))

mycolors <- c("#E8000D", "#0051BA", "#FFC82D", "#355C7D", "#85898A")

get_if_plot <- function(locations = "BB"){
  g_if <- bb_if_data |> 
    filter(UnitArea == locations) |> 
    group_by(Month, Components) |> 
    count(name = "Count") |> 
    ggplot() +
    geom_col(aes(x = Month, y = Count, fill = Components)) +
    labs(title = "Transfusions") +
    scale_fill_manual(values = mycolors)
  return(g_if)
}

diabetesRisk <-function(glucose){
  glucose/200
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$g_if <- renderPlot({
    get_if_plot(input$locations)
  })

  output$inputValue <- renderPrint({input$glucose})
  output$prediction <- renderPrint({diabetesRisk(input$glucose)
    })

})
