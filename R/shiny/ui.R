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
          condition = "input.source_panel == 'Base'",
          radioButtons("base_plot_trigger", "Plot type:",
            c("Histogram" = "histogram",
              "Scatterplot" = "scatterplot"),
            inline=TRUE
          ),
          conditionalPanel(
            condition = "input.base_plot_trigger == 'histogram'",
            sliderInput("binwidth", label="Bin width",
              min=0, max=5, value=1.15, step=.05),
            checkboxInput('dnorm', 'Show density'),
            checkboxInput('density', 'Show normal density'),
            selectInput("rule", label="Rule type",
              c("Sturges" = "sturges",
                "Scott's" = "scott",
                "Freedman-Diaconis" = "fd")
            )
          ),
          selectInput("ntest", label="Normality test",
            c("Shapiro-Wilk" = "shapiro",
              "Pearson Chi^2" = "pearson",
              "Kolmogorov-Smirnov" = "ks")
          )
        ),
        conditionalPanel(
          condition = "input.source_panel = 'Correlation'"
        ),
        uiOutput("overview_ui"),
        uiOutput("scatter_ui")
      ),
      
      mainPanel(
        tabsetPanel(
          id="source_panel",
          tabPanel("Data",
            br(),
            dataTableOutput("datasource")
          ),
          
          tabPanel("Overview",
            br(),
            ggvisOutput("overview")
          ),
          
          tabPanel("Base",
            plotOutput("base_plot"),
            fluidRow(
              column(5,
                conditionalPanel(
                  condition = "input.base_plot_trigger == 'histogram'",
                  h4("Suggested bin width"),
                  textOutput("rule")
                ),
                
                h4("Normality test"),
                htmlOutput("normality")
              ),
              column(2),
              column(5,
                tableOutput("dstats")
              )
            )
          ),
          
          tabPanel("Correlation",
            ggvisOutput("scatterplot"),
            fluidRow(
              column(4,
                h4("Correlation Coefficient"),
                textOutput("correlation")
              ),
              column(1),
              column(7,
                h4("Correlation Significanse"),
                htmlOutput("ctest")
              )
            )
          ),
          
          tabPanel("Regression",
            ggvisOutput("regression"),
            fluidRow(
              column(4,
                h4("Linear Model"),
                uiOutput("lm")
              )
            )
          )
          
        )
      )
    )
  ),
  
  tabPanel("Residuals")
  
))