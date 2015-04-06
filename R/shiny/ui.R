library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Анализ Данных", 
  tabPanel("Исходные данные",
    
    sidebarLayout(
      sidebarPanel(
        helpText("Выберите диапазон наблюдений"),
        sliderInput("range", label="Диапазон",
          min=1, max=38, value=c(1,35)),
        conditionalPanel(
          condition = "input.source_panel == 'Первичный анализ'",
          radioButtons("base_plot_trigger", "График:",
            c("Гистограмма" = "histogram",
              "Диаграмма рассеяния" = "scatterplot"),
            inline=TRUE
          ),
          conditionalPanel(
            condition = "input.base_plot_trigger == 'histogram'",
            sliderInput("binwidth", label="Ширирна столбца",
              min=0, max=5, value=1.15, step=.05),
            checkboxInput('density', 'Отображать плотность распределения'),
            checkboxInput('dnorm', 'Отображать плотность нормального распределения'),
            selectInput("rule", label="Правило",
              c("Стёржеса" = "sturges",
                "Скотта" = "scott",
                "Фридмана-Дьякона" = "fd")
            )
          ),
          selectInput("ntest", label="Критерий нормальность",
            c("Шапиро-Уилка" = "shapiro",
              "Пирсона Хи-квадрать" = "pearson",
              "Колмогорова Смирнова" = "ks")
          )
        ),
        conditionalPanel(
          condition = "input.source_panel == 'Регрессионный анализ'",
          radioButtons("residuals_trigger", "Удалить тренд:",
            c("Нет" = "src",
              "Да" = "residuals"),
            inline=TRUE
          )
        ),
        uiOutput("overview_ui"),
        uiOutput("scatter_ui")
      ),
      
      mainPanel(
        tabsetPanel(
          id="source_panel",
          tabPanel("Данные",
            br(),
            dataTableOutput("datasource")
          ),
          
          tabPanel("Обзор",
            br(),
            ggvisOutput("overview")
          ),
          
          tabPanel("Первичный анализ",
            plotOutput("base_plot"),
            fluidRow(
              column(5,
                conditionalPanel(
                  condition = "input.base_plot_trigger == 'histogram'",
                  h4("Рекомендуемая ширина столбца"),
                  textOutput("rule")
                ),
                
                h4("Критерий нормальности"),
                htmlOutput("normality")
              ),
              column(2),
              column(5,
                tableOutput("dstats")
              )
            )
          ),
          
          tabPanel("Корреляционный анализ",
            ggvisOutput("scatterplot"),
            fluidRow(
              column(4,
                h4("Коэффициент корреляции"),
                textOutput("correlation")
              ),
              column(1),
              column(7,
                h4("Значимость коэффициента корреляции"),
                htmlOutput("ctest")
              )
            )
          ),
          
          tabPanel("Регрессионный анализ",
            ggvisOutput("regression"),
            fluidRow(
              column(4,
                h4("Линейная модель"),
                uiOutput("lm")
              )
            )
          )
          
        )
      )
    )
  ),
  
  tabPanel("Анализ остатков")
  
))