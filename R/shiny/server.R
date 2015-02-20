library(shiny)
library(ggplot2)  # eye-candy graphs
library(ggvis)
library(dplyr)

source("../lib/dstats.R")     # descriptive statistics module

## Read the data / pattern: year;temperature
path.data <- "../../data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

kMinYear <- min(src.data$year)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  series <- reactive({src.data[input$range[1]:input$range[2],]})
  breaks <- reactive({src.data[input$range[1]:input$range[2],1]})

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
    ggplot(series(), aes(x=temperature), geom='blank') +   
      geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=1.2) +
      stat_function(fun=dnorm, colour='red', arg=list(mean=mean(series()$temperature), sd=sd(series()$temperature))) +    
      labs(color="") + xlab("Температура, ºС") + ylab("Плотность") + theme_bw()
  })
  
  
  output$dstats <- renderTable({
    dstats.describe(series()$temperature)
  })
})