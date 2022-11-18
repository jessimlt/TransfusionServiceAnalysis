
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

##most recent year
file_id <- as.numeric(format(Sys.Date(), '%Y'))-1

##stop summarise message
options(dplyr.summarise.inform = FALSE)

##read data
##query in tsa_query
##data prep in tsa_shiny_data_prep
bb_if_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_if_data_fy", file_id, ".rds"))
bb_is_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_is_data_fy", file_id, ".rds"))
bb_t_data <- readRDS(file = paste0("../tsa_shiny/Data/bb_t_data_fy", file_id, ".rds"))

mycolors <- c("#E8000D", "#0051BA", "#FFC82D", "#355C7D", "#85898A")

##Product by Location Tab

get_if_plot <- function(locations){
  g_if <- bb_if_data |> 
    filter(UnitArea == locations) |> 
    group_by(Month, Components) |> 
    count(name = "Count") |> 
    ggplot() +
    geom_col(aes(x = Month, y = Count, fill = Components)) +
    labs(title = "Products Transfused") +
    scale_fill_manual(values = mycolors)
  return(g_if)
}

get_is_plot <- function(locations){
  g_is <- bb_is_data |> 
    filter(UnitArea == locations) |> 
    group_by(Month, Components) |> 
    count(name = "Count") |> 
    ggplot() +
    geom_col(aes(x = Month, y = Count, fill = Components)) +
    labs(title = "Products Issued") +
    scale_fill_manual(values = mycolors)
  return(g_is)
}

get_ifvis_plot <- function(locations){
  
  monthly_if_count <- bb_if_data |>
    filter(UnitArea == locations) |> 
    group_by(Month) |> 
    count(name = "Transfused")
  
  monthly_is_count <-  bb_is_data |> 
    filter(UnitArea == locations) |> 
    group_by(Month) |> 
    count(name = "Issued")
  
  monthly_product_counts <- left_join(monthly_if_count, 
                                      monthly_is_count, 
                                      by = c("Month")) |> 
    pivot_longer(-c(Month), names_to = "Status", values_to = "Count")
  
  g_ifvis <- ggplot(monthly_product_counts) +
    geom_col(aes(x = Month, y = Count, fill = Status),
             position = "dodge") +
    labs(title = "Blood Product Comparison") +
    scale_fill_manual(values = mycolors)

  return(g_ifvis)
}

##Antibody Screens by Order Tab
get_t_counts_plot <- function(ordercodes){
  g_t_counts <- bb_t_data |> 
    filter(Battery == ordercodes) |> 
    group_by(Month, Method) |> 
    count(name = "Count") |> 
    ggplot() +
    geom_col(aes(x = Month, y = Count, fill = Method)) +
    labs(title = "Antibody Screen Counts by Method") +
    scale_fill_manual(values = mycolors)
  return(g_t_counts)
}

get_t_method_plot <- function(ordercodes){
  g_t_method <- bb_t_data |> 
    filter(Battery == ordercodes) |> 
    group_by(Hour, Method) |> 
    summarize(percentile =  quantile(TAT, probs = 0.9)) |> 
    ggplot() +
    geom_line(aes(x = Hour, y = percentile, color = Method)) +
    labs(title = "Antibody Screen TAT by Method",
         x = "Receipt Hour",
         y = "90th percentile minute TAT") +
    scale_x_continuous(breaks = seq(1,23,2)) +
    scale_color_manual(values = mycolors)
  return(g_t_method)
}

get_t_results_plot <- function(ordercodes){
  g_t_results <- bb_t_data |> 
    filter(Battery == ordercodes) |> 
    group_by(Hour, Results) |> 
    summarize(percentile =  quantile(TAT, probs = 0.9)) |> 
    ggplot() +
    geom_line(aes(x = Hour, y = percentile, color = Results)) +
    labs(title = "Antibody Screen TAT by Result Category",
         x = "Receipt Hour",
         y = "90th percentile minute TAT") +
    scale_x_continuous(breaks = seq(1,23,2)) +
    scale_color_manual(values = mycolors)
  return(g_t_results)
}

get_t_priority_plot <- function(ordercodes){
  g_t_priority <- bb_t_data |> 
    filter(Battery == ordercodes) |> 
    group_by(Hour, Priority) |> 
    summarize(percentile =  quantile(TAT, probs = 0.9)) |> 
    ggplot() +
    geom_line(aes(x = Hour, y = percentile, color = Priority)) +
    labs(title = "Antibody Screen TAT by Priority",
         x = "Receipt Hour",
         y = "90th percentile minute TAT") +
    scale_x_continuous(breaks = seq(1,23,2)) +
    scale_color_manual(values = mycolors)
  return(g_t_priority)
}

##server logic
shinyServer(function(input, output) {

  #Product by Location Tab  
  output$g_if <- renderPlotly({
    get_if_plot(input$locations)
  })
  
  output$g_is <- renderPlotly({
    get_is_plot(input$locations)
  })
  
  output$g_ifvis <- renderPlotly({
    get_ifvis_plot(input$locations)
  })

  #Antibody Screens by Order
  output$g_t_counts <- renderPlotly({
    get_t_counts_plot(input$ordercodes)
  })
  output$g_t_results <- renderPlotly({
    get_t_results_plot(input$ordercodes)
  })
  output$g_t_method <- renderPlotly({
    get_t_method_plot(input$ordercodes)
  })
  output$g_t_priority <- renderPlotly({
    get_t_priority_plot(input$ordercodes)
  })

})
