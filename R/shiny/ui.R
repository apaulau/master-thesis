library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Temperature Analysis", 
  tabPanel("Source",
    sidebarLayout(
      sidebarPanel(
        helpText("Choose the range of observations"),
        sliderInput("range", label="Range",
          min=1, max=38, value=c(1,35)),
        uiOutput("series_ui")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data",
            ggvisOutput("series"),
            tableOutput("series_table"),
            textOutput("text")
          ),
          tabPanel("Histogram",
            plotOutput("histogram"),
            tableOutput("dstats")
          )
        )
      )
    )
  )
))