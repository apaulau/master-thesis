library(shiny)
library(ggplot2)  # eye-candy graphs
library(ggvis)
library(dplyr)

## Read the data / pattern: year;temperature
path.data <- "../../data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

kMinYear <- min(src.data$year)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$range <- renderText({
    paste("You've changed range: from", input$range[1], "to", input$range[2])
  })
  
  series <- reactive({src.data[input$range[1]:input$range[2],]})
  #breaks <- reactive({input$range[1]:input$range[2],1]})
  
  series %>% ggvis(~year, ~temperature) %>% layer_points() %>% layer_lines() %>% 
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>% 
    bind_shiny("series", "ggvis_ui")
  
  output$series_table <- renderTable({
    series()
  })
})