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
        tableOutput("series_table"),
        uiOutput("series_ui")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data",
            ggvisOutput("series"),
            textOutput("text")
          ),
          tabPanel("Histogram",
            plotOutput("histogram"),
            sliderInput("binwidth", label="Bin width",
              min=0, max=5, value=1.15, step=.05),
            helpText("Suggested width"),
            textOutput("sturges"),
            textOutput("scott"),
            textOutput("fd"),
            tableOutput("dstats")
          )
        )
      )
    )
  )
))