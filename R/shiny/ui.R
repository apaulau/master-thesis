library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Shiny Server"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose the range of observations"),
      sliderInput("range", label="Range",
                  min=1, max=38, value=c(1,35))
    ),
    mainPanel(
      h2("Range"),
      textOutput("range"),
      h3("Plot"),
      plotOutput("series")
    )
  )
))