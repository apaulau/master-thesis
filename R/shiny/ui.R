library(shiny)
library(ggvis)
require(markdown)

shinyUI(navbarPage("Анализ Баторино",  id="nav", 
  tabPanel("Исходные данные",
    includeCSS("styles.css"),  
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
              column(1),
              column(6,
                dataTableOutput("dstats")
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
              column(1),
              column(6,
                dataTableOutput("residual_dstats")
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
        sliderInput("variogram_range", label="Диапазон",
          min=1, max=38, value=c(1,35)),
        conditionalPanel(
          condition = "input.variogram_panel == 'Вариограмма' | input.variogram_panel == 'Кригинг' | input.variogram_panel == 'Подбор параметров'",
          numericInput("cutoff", "Максимальный лаг", value=1, min=0, max=38, step=1),
          conditionalPanel(
            condition = "input.afv == false",
            selectInput("modelV", "Модель вариограммы",
              c(#"Эффект самородков"="Nug",
                "Сферическая"="Sph",
                "Экспоненциальная"="Exp",
                "Гауссовская"="Gau",
                "Круговая"="Cir",
                "Линейная"="Lin",
                "Бесселя"="Bes",
                "Пентасферическая"="Pen",
                "Периодическая"="Per",
                "Волновая"="Wav",
                "С эффектом дыр"="Hol",
                "Логарифмическая"="Log",
                "Сплайн"="Spl")
            ),       
            numericInput("nugget", "Самородок", value=0, min=0),
            numericInput("rangeV",  "Ранг", value=1, min=.1, step=.1),
            numericInput("psill", "Порог", value=1, min=.1, step=.1),
            checkboxInput("fitVariogram", "Подогнать параметры", value=TRUE)
          ),
          checkboxInput("cressie", "Использовать оценку Кресси"),
          conditionalPanel(
            condition = "input.variogram_panel == 'Вариограмма' | input.variogram_panel == 'Кригинг'",
            checkboxInput("afv", "Автоматический подбор модели")
          ),
          conditionalPanel(
            condition = "input.variogram_panel == 'Кригинг'",
            numericInput("future", "Будущее", value=0, min=0, max=38, step=1)
          )
        ),
        conditionalPanel(
          condition = "input.variogram_panel == 'Подбор параметров' | input.variogram_panel == 'Сравнительный анализ'",
          selectInput("measure", label="Мера",
            c("MAE"  = "MAE",
              "MSE"  = "MSE",
              "RMSE" = "RMSE")
          )
        ),
        conditionalPanel(
          condition = "input.variogram_panel == 'Подбор параметров'",
          radioButtons("fit_param", "Параметр",
            choices = list("Максимальный лаг" = 1, "Наггет" = 2, "Порог" = 3,
              "Ранг" = 4), selected = 1, inline=TRUE),
          numericInput("fit_min", "Минимум",  value=.1, min=.1, step=.1),
          numericInput("fit_max", "Максимум", value=10, min=.1, step=.1),
          numericInput("fit_step", "Шаг", value=.1, min=.1, step=.1),
          actionButton('fitParam', 'Подобрать')
        ),
        conditionalPanel(
          condition = "input.variogram_panel == 'Сравнительный анализ'",
          actionButton('computeComparison', 'Сравнить')
        )
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
          
          tabPanel("Подбор параметров",
            br(),
            plotOutput("fit_param", height=500),
            fluidRow(
                htmlOutput("fit_mse")
            )
          ),
          
          tabPanel("Кригинг",
            br(),
            ggvisOutput("cross_prediction"),
            fluidRow(
              column(4,       
                h4("Кригинг"),
                dataTableOutput("predictions")
              ),
              column(1),
              column(6,
                h4("Анализ"),
                dataTableOutput("analysis")
              )
            )
          ),
          
          tabPanel("Сравнительный анализ",
            br(),
            plotOutput("param_comparison", height=500),
            fluidRow(
              column(5,       
                #h4("Лучшие значения"),
                dataTableOutput("best_cutoff")
              ),
              column(2),
              column(5,
                htmlOutput("something2")
              )
            )
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