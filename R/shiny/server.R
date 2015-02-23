library(shiny)
library(ggplot2)  # eye-candy graphs
library(ggvis)
library(dplyr)

source("../lib/dstats.R")     # descriptive statistics module
source("../lib/draw.R")       # helpers for drawing

## Read the data / pattern: year;temperature
path.data <- "../../data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

kMinYear <- min(src.data$year)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  series <- reactive({src.data[input$range[1]:input$range[2],]})
  breaks <- reactive({src.data[input$range[1]:input$range[2],1]})
  binwidth <- reactive({input$binwidth})
  sturges <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.Sturges(data)
  })
  scott <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.FD(data)
  })
  fd <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.scott(data)
  })

  series %>% ggvis(~year, ~temperature) %>% layer_points() %>% layer_paths() %>% 
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("series", "series_ui")
  
  output$series_table <- renderTable({
    #-webkit-column-count=x
    series()
  })
  output$histogram <- renderPlot({
    datebreaks <- seq(kMinYear - 5 + input$range[1], kMinYear + 5 + input$range[2], by=2)
    width <- binwidth()
    if (width > 0) {
      DrawHistogram(data=series(), binwidth=width)
    } else {
      DrawHistogram(data=series())
    }
    #ifelse(width > 0, , )
  })
  
  output$sturges <- renderText({
    paste("Sturges rule:", sturges())
  })
  
  output$scott <- renderText({
    paste("Scott's choice for a normal distribution:", scott())
  })
  
  output$fd <- renderText({
    paste("Freedman-Diaconis choice:", fd())
  })
  
  output$dstats <- renderTable({
    dstats.describe(series()$temperature)
  })
})