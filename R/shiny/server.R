library(shiny)
library(ggplot2)  # eye-candy graphs
library(ggvis)
library(dplyr)

source("../lib/dstats.R")     # descriptive statistics module
source("../lib/draw.R")       # helpers for drawing
source("../lib/ntest.R")       # helpers for drawing

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
  
  output$series_table <- renderDataTable({
    #-webkit-column-count=x
    series()
  })
  
  output$base_plot <- renderPlot({
    plot <- NA
    
    if (input$base_plot_trigger == "histogram") {
      datebreaks <- seq(kMinYear - 5 + input$range[1], kMinYear + 5 + input$range[2], by=2)
      width <- binwidth()
      
      if (width > 0) {
        plot <- DrawHistogram(data=series(), binwidth=width, fit=input$dnorm)
      } else {
        plot <- DrawHistogram(data=series(), fit=input$dnorm)
      }
      
      if (input$density) {
        plot <- plot + geom_density(colour="#999999", fill="#009E73", alpha=.5)
      } 
    } else {
      plot <- DrawQuantileQuantile(data=series()$temperature)
    }
    
    plot
    #ifelse(width > 0, , )
  })
  
  output$rule <- renderText({
    rule <- switch(input$rule,
                   sturges = sturges,
                   scott = scott,
                   fd = fd)
    
    format(rule(), digits=3)
  })
  
  output$dstats <- renderTable({
    dstats.describe(series()$temperature)
  })
  
  output$normality <- renderUI({
    test <- switch(input$ntest,
                   shapiro = ntest.ShapiroWilk,
                   pearson = ntest.PearsonChi2,
                   ks = ntest.KolmogorovSmirnov)
    
    result <- test(data=series()$temperature)
    statistic <- paste("<b>Statistic:</b>", format(result$statistic[[1]]))
    p.value <- paste("<b>P-value:</b>", format(result$p.value))
    conclusion <- paste(ifelse(p.value <= .05, "Null hypothesis is rejected.", "Failed to reject null hypothesis"))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  output$correlation <- renderPlot({
    
  })
  
})