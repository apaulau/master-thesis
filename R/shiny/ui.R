library(shiny)
library(shinydashboard)

library(ggvis)
require(markdown)

header <- dashboardHeader(title = "Анализ о.Баторино")
sidebar <- dashboardSidebar(sidebarMenu(
    sliderInput(
        "range",
        label = "Диапазон",
        min = 1,
        max = 100,
        value = c(1, 32)
    ),
    menuItem(
        "Исходные данные",
        tabName = "source",
        icon = icon("database")
    ),
    menuItem(
        "Разведочный анализ",
        icon = icon("line-chart"),
        menuSubItem("Первичный анализ", tabName = "initialSource"),
        menuSubItem("Корреляционный анализ", tabName = "corrSource"),
        menuSubItem("Регрессионный анализ", tabName = "regrSource")
    ),
    menuItem("Остатки", tabName = "residuals", icon = icon("table")),
    menuItem(
        "Анализ остатков",
        icon = icon("bar-chart"),
        menuSubItem("Первичный анализ", tabName = "initialResiduals"),
        menuSubItem("Автокорреляционная функция", tabName = "acfResiduals")
    ),
    menuItem(
        "Вариограммный анализ",
        tabName = "variogram_analysis",
        icon = icon("area-chart")
    )
))

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "source",
                fluidRow(
                    box(
                        title = "Данные",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 5,

                        dataTableOutput("datasource")
                    ),
                    box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 7,

                        ggvisOutput("overview")
                    )
                )),
        tabItem(tabName = "initialSource",
                fluidRow(
                    column(
                        width = 3,
                        box(
                            title = "Параметры",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,

                            conditionalPanel(
                                condition = "input.base_plot == 'Гистограмма'",
                                sliderInput(
                                    "binwidth",
                                    label = "Ширина столбца",
                                    min = 0,
                                    max = 5,
                                    value = 1.15,
                                    step = .05
                                ),
                                checkboxInput('density', 'Отображать плотность распределения'),
                                checkboxInput('dnorm', 'Отображать плотность нормального распределения'),
                                selectInput(
                                    "rule",
                                    label = "Правило",
                                    c(
                                        "Стёрджеса" = "sturges",
                                        "Скотта" = "scott",
                                        "Фридмана-Дьякона" = "fd"
                                    )
                                )
                            ),
                            textOutput("rule")
                        ),
                        box(
                            title = "Проверка на нормальность",
                            solidHeader = TRUE,
                            collapsible = TRUE,

                            width = 12,

                            selectInput(
                                "ntest",
                                label = "Критерий",
                                c(
                                    "Шапиро-Уилка" = "shapiro",
                                    "Пирсона Хи-квадрат" = "pearson",
                                    "Колмогорова Смирнова" = "ks"
                                )
                            ),
                            htmlOutput("normality")
                        )
                    ),
                    column(
                        6,
                        tabBox(
                            title = "График",
                            id = "base_plot",
                            width = 12,
                            tabPanel("Гистограмма",
                                     plotOutput("histSource")),
                            tabPanel("Квантиль-Квантиль",
                                     plotOutput("qqSource"))
                        )
                    ),
                    column(
                        3,
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Описательные статистики",
                            width = 12,

                            dataTableOutput("dstats")
                        )
                    )

                )),
        tabItem(tabName = "corrSource",
                fluidRow(
                    column(
                        width = 7,
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Корреляционный анализ",
                            width = 12,
                            ggvisOutput("scatterplot")
                        )
                    ),
                    column(
                        width = 5,
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Коэффициент корреляции",
                            width = 12,
                            textOutput("correlation")
                        ),
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Значимость коэффициента корреляции",
                            width = 12,
                            htmlOutput("ctest")
                        )
                    )
                )),
        tabItem(tabName = "regrSource",
                fluidRow(
                    column(
                        7,
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "График",
                            width = 12,

                            ggvisOutput("regression"),
                            hr(),
                            radioButtons(
                                "residuals_trigger",
                                "Удалить тренд:",
                                c("Нет" = "src",
                                  "Да" = "residuals"),
                                inline = TRUE
                            )
                        )
                    ),
                    column(
                        5,
                        box(
                            title = "Модель",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,

                            uiOutput("lm")
                        ),
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            "Адекватность модели",
                            width = 12,
                            htmlOutput("adequacy")
                        ),
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            "Значимость модели",
                            width = 12,
                            htmlOutput("signif")
                        )
                    )

                )),
        tabItem(tabName = "residuals",
                fluidRow(
                    box(
                        title = "Данные",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 5,

                        dataTableOutput("residual_source")
                    ),
                    box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 7,

                        ggvisOutput("residual_overview")
                    )
                )),
        tabItem(tabName = "initialResiduals",
                fluidRow(
                    column(
                        width = 3,
                        box(
                            title = "Параметры",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,

                            conditionalPanel(
                                condition = "input.residual_plot == 'Гистограмма'",
                                sliderInput(
                                    "residual_binwidth",
                                    label = "Ширина столбца",
                                    min = 0,
                                    max = 5,
                                    value = 1.15,
                                    step = .05
                                ),
                                checkboxInput('residual_density', 'Отображать плотность распределения'),
                                checkboxInput(
                                    'residual_dnorm',
                                    'Отображать плотность нормального распределения'
                                ),
                                selectInput(
                                    "residual_rule",
                                    label = "Правило",
                                    c(
                                        "Стёрджеса" = "sturges",
                                        "Скотта" = "scott",
                                        "Фридмана-Дьякона" = "fd"
                                    )
                                ),
                                textOutput("residual_rule")
                            )
                        ),
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Проверка на нормальность",
                            width = 12,

                            selectInput(
                                "residual_ntest",
                                label = "Критерий",
                                c(
                                    "Шапиро-Уилка" = "shapiro",
                                    "Пирсона Хи-квадрат" = "pearson",
                                    "Колмогорова Смирнова" = "ks"
                                )
                            ),
                            htmlOutput("residual_normality")
                        )
                    ),
                    column(
                        6,
                        tabBox(
                            title = "График",
                            id = "residual_plot",
                            width = 12,
                            tabPanel("Гистограмма",
                                     plotOutput("histResiduals")),
                            tabPanel("Квантиль-Квантиль",
                                     plotOutput("qqResiduals"))
                        )
                    ),
                    column(
                        3,
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Описательные статистики",
                            width = 12,

                            dataTableOutput("residual_dstats")
                        )
                    )

                )),
        tabItem(tabName = "acfResiduals",
                fluidRow(

                    column(8,
                           box(title = "Автокорреляционная функция",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = 12,

                               plotOutput("acf", height = 600))
                    ),
                    column(4,
                           box(
                               title = "Тест Льюнга-Бокса",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = 12,

                               htmlOutput("ljung")
                           ),
                           box(
                               title = "Расширенный тест Дики-Фуллера",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = 12,
                               htmlOutput("adf")
                           )
                    )
                )),
        tabItem(tabName = "variogram_analysis",
                fluidRow(
                    box(
                        title = "Параметры",
                        width = 3,
                        solidHeader = TRUE,
                        collapsible = TRUE,

                        conditionalPanel(
                            condition = "input.variogram_panel == 'Семивариограмма' | input.variogram_panel == 'Кригинг' | input.variogram_panel == 'Подбор параметров' | input.variogram_panel == 'Кросс-валидация'",
                            numericInput(
                                "cutoff",
                                "Максимальный лаг",
                                value = 1,
                                min = 0,
                                max = 38,
                                step = 1
                            ),
                            conditionalPanel(
                                condition = "input.afv == false",
                                selectInput(
                                    "modelV",
                                    "Модель семивариограммы",
                                    c(
                                        #"Эффект самородков"="Nug",
                                        "Сферическая" = "Sph",
                                        "Экспоненциальная" = "Exp",
                                        "Гауссовская" = "Gau",
                                        "Круговая" = "Cir",
                                        "Линейная" = "Lin",
                                        "Бесселя" = "Bes",
                                        "Пентасферическая" = "Pen",
                                        "Периодическая" = "Per",
                                        "Волновая" = "Wav",
                                        "С эффектом дыр" = "Hol",
                                        "Логарифмическая" = "Log",
                                        "Сплайн" = "Spl"
                                    )
                                ),
                                numericInput("nugget", "Самородок", value = 0, min = 0),
                                numericInput(
                                    "rangeV",
                                    "Ранг",
                                    value = 1,
                                    min = .1,
                                    step = .1
                                ),
                                numericInput(
                                    "psill",
                                    "Порог",
                                    value = 1,
                                    min = .1,
                                    step = .1
                                ),
                                checkboxInput("fitVariogram", "Подогнать параметры", value =
                                                  TRUE)
                            ),
                            checkboxInput("cressie", "Использовать оценку Кресси"),
                            conditionalPanel(
                                condition = "input.variogram_panel == 'Семивариограмма' | input.variogram_panel == 'Кригинг' | input.variogram_panel == 'Кросс-валидация'",
                                checkboxInput("afv", "Автоматический подбор модели")
                            ),
                            conditionalPanel(
                                condition = "input.variogram_panel == 'Кригинг'",
                                numericInput(
                                    "future",
                                    "Будущее",
                                    value = 0,
                                    min = 0,
                                    max = 38,
                                    step = 1
                                )
                            )
                        ),
                        conditionalPanel(
                            condition = "input.variogram_panel == 'Подбор параметров' | input.variogram_panel == 'Сравнительный анализ'",
                            checkboxInput("cross", "Кросс-валидация"),
                            conditionalPanel(condition = "!input.cross",
                                             selectInput(
                                                 "measure", label = "Мера",
                                                 c(
                                                     "MAE"  = "MAE",
                                                     "MSE"  = "MSE",
                                                     "RMSE" = "RMSE"
                                                 )
                                             )),
                            conditionalPanel(condition = "input.cross",
                                             selectInput(
                                                 "cvm",
                                                 label = "Мера",
                                                 c(
                                                     "MAE"  = "MAE",
                                                     "RSS"  = "RSS",
                                                     "MSE"  = "MSE",
                                                     "RMSE" = "RMSE",
                                                     "Корреляция" = "cor_obspred"
                                                 )
                                             ))

                        ),
                        conditionalPanel(
                            condition = "input.variogram_panel == 'Подбор параметров'",
                            radioButtons(
                                "fit_param",
                                "Параметр",
                                choices = list(
                                    "Максимальный лаг" = 1,
                                    "Наггет" = 2,
                                    "Порог" = 3,
                                    "Ранг" = 4
                                ),
                                selected = 1,
                                inline = TRUE
                            ),
                            numericInput(
                                "fit_min",
                                "Минимум",
                                value = .1,
                                min = 0,
                                step = .1
                            ),
                            numericInput(
                                "fit_max",
                                "Максимум",
                                value = 10,
                                min = .1,
                                step = .1
                            ),
                            numericInput(
                                "fit_step",
                                "Шаг",
                                value = .1,
                                min = .1,
                                step = .1
                            ),
                            actionButton('fitParam', 'Подобрать')
                        ),
                        conditionalPanel(condition = "input.variogram_panel == 'Сравнительный анализ'",
                                         actionButton('computeComparison', 'Сравнить')),
                        conditionalPanel(
                            condition = "input.variogram_panel == 'Кросс-валидация'",
                            numericInput(
                                "nfold",
                                "nfold",
                                value = 32,
                                min = 2,
                                max = 38
                            )
                        )
                    ),
                    tabBox(
                        id = "variogram_panel",
                        width = 9,

                        tabPanel(
                            "Семивариограмма",
                            br(),
                            ggvisOutput("variogram"),
                            fluidRow(column(
                                5,
                                h4("Модель семивариограммы"),
                                htmlOutput("text_model")
                            ),
                            column(2),
                            column(5,
                                   htmlOutput("sserr")))
                        ),

                        tabPanel(
                            "Подбор параметров",
                            br(),
                            plotOutput("fit_param", height = 900),
                            fluidRow(htmlOutput("fit_mse"))
                        ),

                        tabPanel(
                            "Кригинг",
                            br(),
                            ggvisOutput("cross_prediction"),
                            fluidRow(
                                column(4,
                                       h4("Кригинг"),
                                       dataTableOutput("predictions")),
                                column(1),
                                column(6,
                                       h4("Анализ"),
                                       dataTableOutput("analysis"))
                            ),
                            h4("Кросс-валидация"),
                            dataTableOutput("cv_stats")
                        ),

                        tabPanel(
                            "Сравнительный анализ",
                            br(),
                            plotOutput("param_comparison", height = 700),
                            fluidRow(column(5,
                                            #h4("Лучшие значения"),
                                            dataTableOutput("best_cutoff")),
                                     column(2),
                                     column(5,
                                            htmlOutput("something2")))
                        ),

                        tabPanel(
                            "Кросс-валидация",
                            br(),
                            #plotOutput("param_comparison", height=500),
                            ggvisOutput("cv_plot"),
                            fluidRow(
                                column(6,
                                       dataTableOutput("cv")),
                                column(1),
                                column(5,
                                       dataTableOutput("cv_stats2"))
                            )
                        )
                    )
                ))
    )
)

shinyUI(dashboardPage(header, sidebar, body))

#   navbarMenu("О программе",
#     tabPanel("Введение",
#       fluidPage(
#         fluidRow(
#           column(2),
#           column(8,
#             includeMarkdown("intro.md")
#           ),
#           column(2)
#         )
#       )
#     ),
#     tabPanel("Список дел",
#       fluidPage(
#         fluidRow(
#           column(2),
#           column(8,
#             includeMarkdown("include/TODO.Rmd")
#           ),
#           column(2)
#         )
#       )
#     )
#     )
# ))
