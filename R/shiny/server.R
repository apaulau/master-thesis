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
  datebreaks <- reactive({
    seq(kMinYear - 5 + input$range[1], kMinYear + 5 + input$range[2], by=2)
  })
  model <- reactive({
    lm(series()$temperature ~ c(input$range[1]:input$range[2]))
  })
  
  output$datasource <- renderDataTable({
    series()
  })

  series %>% ggvis(~year, ~temperature) %>% layer_points() %>% layer_paths() %>% 
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("overview", "overview_ui")
  
  output$base_plot <- renderPlot({
    plot <- NA
    
    if (input$base_plot_trigger == "histogram") {
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
    conclusion <- paste(ifelse(result$p.value <= .05, "Null hypothesis is rejected.", "Failed to reject null hypothesis"))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  series %>% ggvis(~year, ~temperature) %>% 
    layer_points() %>% # could be customizable, e.g. by size, fill
    layer_model_predictions(model="lm", se=FALSE, stroke := "#0072B2") %>%
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("scatterplot", "scatter_ui")
  
  output$correlation <- renderText({
    format(cor(series()$temperature, c(1:(input$range[2] - input$range[1] + 1))), digits=5)
  })
  
  output$ctest <- renderUI({    
    result <- cor.test(series()$temperature, c(1:(input$range[2] - input$range[1] + 1)), method="pearson")
    statistic <- paste("<b>Statistic:</b>", format(result$statistic[[1]]))
    df <- paste("<b>Degrees of freedom:</b>", format(result$parameter[["df"]]))
    p.value <- paste("<b>P-value:</b>", format(result$p.value))
    ci <- paste("<b>", attr(test$conf.int, "conf.level") * 100, "percent confidence interval:</b>", "[", format(test$conf.int[1], digits=4), ";", format(test$conf.int[2], digits=4), "]")
    conclusion <- paste(ifelse(result$p.value <= .05, "Null hypothesis (correlation equals 0) is rejected.", "Failed to reject null hypothesis (correlation equals 0)"))
    HTML(paste(statistic, p.value, df, ci, conclusion, sep = '<br/>'))
  })
  
  linear <- function(x, a, b) a * x + b
  line <- reactive({
    data.frame(
      x_rng = breaks(), 
      y_rng = sapply(c((input$range[1]):(input$range[2])), FUN=linear, a=coef(model())[2], b=coef(model())[1])
    )
  })
  
  mix <- reactive({
    data.frame(series(), line())
  })
  
  mix %>% ggvis(x=~year, y=~temperature) %>%
    layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue") %>%
    layer_points(x=~year, y=~temperature) %>% layer_paths() %>% 
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("regression", "regression_ui")
  
  output$lm <- renderUI({
    m <- model()
    withMathJax(sprintf("$$y = %.03f x + %.03f $$", coef(model())[2], coef(model())[1]))
#     withMathJax(
#       helpText(paste("$$ y = ", format(coef(model())[2], digits=4), "x +", format(coef(model())[2], digits=4), "$$"))  
#     )
  })
})