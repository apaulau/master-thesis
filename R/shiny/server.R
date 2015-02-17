library(shiny)
library(ggplot2)  # eye-candy graphs


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
  
  output$series <- renderPlot({
    datebreaks <- seq(kMinYear - 5 + input$range[1], kMinYear + 5 + input$range[2], by=2)
    ggplot(data=src.data[input$range[1]:input$range[2],], aes(x=year, y=temperature)) + 
      geom_point() + geom_line() + 
      scale_x_continuous(breaks=datebreaks) + 
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      xlab("Year") + ylab("Temperature, ºС")
  })
})