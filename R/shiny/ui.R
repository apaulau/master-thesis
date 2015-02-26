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
        conditionalPanel(
          condition = "input.source_panel == 'Histogram'",
          sliderInput("binwidth", label="Bin width",
            min=0, max=5, value=1.15, step=.05),
          checkboxInput('dnorm', 'Show density'),
          checkboxInput('density', 'Show normal density'),
          selectInput("rule", label="Rule type",
            c("Sturges" = "sturges()",
              "Scott's" = "scott()",
              "Freedman-Diaconis" = "fd()")
          ),
          selectInput("ntest", label="Normality test",
            c("Shapiro-Wilk" = "ntest.ShapiroWilk",
              "Pearson Chi^2" = "ntest.PearsonChi2",
              "Kolmogorov-Smirnov" = "ntest.KolmogorovSmirnov")
          )
        ),
        uiOutput("series_ui")
      ),
      
      mainPanel(
        tabsetPanel(
          id="source_panel",
          tabPanel("Data",
            br(),
            dataTableOutput("series_table")
          ),
          
          tabPanel("Time Series",
            br(),
            ggvisOutput("series")
          ),
          
          tabPanel("Histogram",
            plotOutput("histogram"),
            fluidRow(
              column(5,
                h4("Suggested bin width"),
                textOutput("rule"),
                h4("Normality test"),
                htmlOutput("normality")
              ),
              column(2),
              column(5,
                tableOutput("dstats")
              )
            )
          )
          
        )
      )
    )
  )
))