library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Temperature Analysis", 
  tabPanel("Data",
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose the range of observations"),
      sliderInput("range", label="Range",
                  min=1, max=38, value=c(1,35)),
      uiOutput("ggvis_ui")
    ),
    mainPanel(
      ggvisOutput("series"),
      tableOutput("series_table")
    )
  ))
))