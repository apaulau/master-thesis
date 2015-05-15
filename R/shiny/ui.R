library(shiny)
library(ggvis)
require(markdown)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Анализ Баторино",  id="nav", 
  tabPanel("Исходные данные",
    
    sidebarLayout(
      sidebarPanel(
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
            sliderInput("binwidth", label="Ширина столбца",
              min=0, max=5, value=1.15, step=.05),
            checkboxInput('density', 'Отображать плотность распределения'),
            checkboxInput('dnorm', 'Отображать плотность нормального распределения'),
            selectInput("rule", label="Правило",
              c("Стёрджеса" = "sturges",
                "Скотта" = "scott",
                "Фридмана-Дьякона" = "fd")
            )
          ),
          selectInput("ntest", label="Критерий нормальности",
            c("Шапиро-Уилка" = "shapiro",
              "Пирсона Хи-квадрат" = "pearson",
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
                h4("Модель"),
                uiOutput("lm")
              ),
              column(1),
              column(7,
                h4("Значимость модели"),
                htmlOutput("signif"),
                hr(),
                h4("Адекватность модели"),
                htmlOutput("adequacy")
              )
            )
          )
          
        )
      )
    )
  ),
  
  tabPanel("Анализ остатков",
    sidebarLayout(
      sidebarPanel(
        sliderInput("residual_range", label="Диапазон",
          min=1, max=38, value=c(1,35)),
        conditionalPanel(
          condition = "input.residual_panel == 'Первичный анализ'",
          radioButtons("residual_base_plot_trigger", "График:",
            c("Гистограмма" = "histogram",
              "Диаграмма рассеяния" = "scatterplot"),
            inline=TRUE
          ),
          conditionalPanel(
            condition = "input.residual_base_plot_trigger == 'histogram'",
            sliderInput("residual_binwidth", label="Ширина столбца",
              min=0, max=5, value=1.15, step=.05),
            checkboxInput('residual_density', 'Отображать плотность распределения'),
            checkboxInput('residual_dnorm', 'Отображать плотность нормального распределения'),
            selectInput("residual_rule", label="Правило",
              c("Стёрджеса" = "sturges",
                "Скотта" = "scott",
                "Фридмана-Дьякона" = "fd")
            )
          ),
          selectInput("residual_ntest", label="Критерий нормальности",
            c("Шапиро-Уилка" = "shapiro",
              "Пирсона Хи-квадрат" = "pearson",
              "Колмогорова Смирнова" = "ks")
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          id="residual_panel",
          tabPanel("Данные",
            br(),
            dataTableOutput("residual_source")
          ),
          
          tabPanel("Обзор",
            br(),
            ggvisOutput("residual_overview")
          ),
          
          tabPanel("Первичный анализ",
            plotOutput("residual_plot"),
            fluidRow(
              column(5,
                conditionalPanel(
                  condition = "input.residual_base_plot_trigger == 'histogram'",
                  h4("Рекомендуемая ширина столбца"),
                  textOutput("residual_rule")
                ),
                
                h4("Критерий нормальности"),
                htmlOutput("residual_normality")
              ),
              column(2),
              column(5,
                tableOutput("residual_dstats")
              )
            )
          ),
          tabPanel("Автокорреляционная функция",
            plotOutput("acf"),
            fluidRow(
              column(5,
                h4("Тест Льюнга-Бокса"),
                htmlOutput("ljung")
              ),
              column(2),
              column(5,
                h4("Расширенный тест Дики-Фуллера"),
                htmlOutput("adf")
              )
            )
          )
        )
      )
    )
  ),
  tabPanel("Вариограммный анализ",
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.variogram_panel == 'Вариограмма'",
          numericInput("cutoff", "Максимальный лаг", value=21, min=0, max=38, step=1),
          conditionalPanel(
            condition = "input.afv == false",
            selectInput("modelV", "Модель вариограммы",
              c(#"Эффект самородков"="Nug",
                "Экспоненциальная"="Exp",
                "Сферическая"="Sph",
                "Гауссовская"="Gau",
                "Матерна"="Mat",
                "Штейна"="Ste",
                "Круговая"="Cir",
                "Линейная"="Lin",
                "Бесселя"="Bes",
                "Пентасферическая"="Pen",
                "Периодическая"="Per",
                "Волновая"="Wav",
                "С эффектом дыр"="Hol",
                "Логарифмическая"="Log",
                "Сплайн"="Spl",
                "Лежандра"="Leg")
            ),
            
            numericInput("nugget", "Самородок", value=0, min=0),
            numericInput("rangeV",  "Ранг", value=1, min=.1, step=.1),
            numericInput("psill", "Порог", value=1, min=.1, step=.1),
            checkboxInput("fitVariogram", "Подогнать параметры", value=TRUE)
          ),
          checkboxInput("afv", "Автоматический подбор модели"),
          conditionalPanel(
            condition = "input.afv == true",
            checkboxInput("cressie", "Использовать оценку Кресси")
          )
        ),
        uiOutput("variogram_ui")
      ),
      mainPanel(
        tabsetPanel(
          id="variogram_panel",
          tabPanel("Вариограмма",
            br(),
            ggvisOutput("variogram"),
            fluidRow(
              column(5,             
                h4("Модель вариограммы"),
                htmlOutput("text_model")
              ),
              column(2),
              column(5,
                htmlOutput("sserr")
              )
            )
          ),
          
          tabPanel("Кригинг",
            br(),
            ggvisOutput("krige"),
            fluidRow(
              column(5,             
                h4("Прогноз"),
                tableOutput("predictions")
              ),
              column(2),
              column(5,
                htmlOutput("something")
              )
            )
          ),
          
          tabPanel("Сравнительный анализ"
          )
        )
      )
    )
  ),
  navbarMenu("О программе",
    tabPanel("Введение",
      fluidPage(
        fluidRow(
          column(2),
          column(8,
            includeMarkdown("intro.md")
          ),
          column(2)
        )
      )
    ),
    tabPanel("Список дел",
      fluidPage(
        fluidRow(
          column(2),
          column(8,
            includeMarkdown("include/TODO.Rmd")
          ),
          column(2)
        )
      )
    )
  )
))