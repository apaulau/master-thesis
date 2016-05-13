library(shiny)
library(shinydashboard)

library(ggplot2)
library(ggvis)
library(dplyr)
library(tseries)
library(sp)
library(gstat)
library(reshape2)


source("lib/dstats.R")     # descriptive statistics module
source("lib/draw.R")       # helpers for drawing
source("lib/plot.R")       # custom plots
source("lib/ntest.R")      # normality tests
source("lib/regr.R")       # regression tests
source("lib/afv.R")        # autofit variogram module
source("lib/variogram.R")
source("lib/kriging.R")
source("lib/misc.R")
source("lib/measures.R")
source("lib/getdata.R")

## Read the data
src  <- read()
kObservationNum <- 0
kminRange <- min(src$year)
nrows <- length(src[, 1])

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    minRange <- reactive({
        if (input$nav == "Анализ остатков") {
            input$residual_range[1]
        } else if (input$nav == "Вариограммный анализ") {
            input$variogram_range[1]
        } else {
            input$range[1]
        }
    })
    maxRange <- reactive({
        if (input$nav == "Анализ остатков") {
            input$residual_range[2]
        } else if (input$nav == "Вариограммный анализ") {
            input$variogram_range[2]
        } else {
            input$range[2]
        }
    })
    series <- reactive({
        src[minRange():maxRange(), ]
    })
    breaks <- reactive({
        src[minRange():maxRange(), 1]
    })
    binwidth <- reactive({
        input$binwidth
    })
    sturges <- reactive({
        data <- series()$temperature
        (max(data) - min(data)) / nclass.Sturges(data)
    })
    scott <- reactive({
        data <- series()$temperature
        (max(data) - min(data)) / nclass.FD(data)
    })
    fd <- reactive({
        data <- series()$temperature
        (max(data) - min(data)) / nclass.scott(data)
    })
    datebreaks <- reactive({
        seq(kminRange - 5 + minRange(), kminRange + 5 + maxRange(), by = 2)
    })
    model <- reactive({
        lm(series()$temperature ~ c(minRange():maxRange()))
    })
    residuals <- reactive({
        data.frame(year = series()$year,
                   temperature = model()$residuals)
    })

    output$datasource <- renderDataTable({
        df <- series()
        colnames(df) <- c("Год наблюдения", "Температура", "Растворимость О2", "Насыщенность")
        df
    })

    series %>% ggvis( ~year, ~temperature) %>% layer_points() %>% layer_paths() %>%
        add_axis(
            "x",
            title = "Год наблюдения",
            format = "d",
            properties = axis_props(labels = list(angle = 45, align = "left"))
        ) %>%
        add_axis("y", title = "Температура, ºС") %>%
        add_tooltip(function(df)
            paste(df$year, ":", df$temperature)) %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("overview", "overview_ui")

    output$base_plot <- renderPlot({
        plot <- NA

        if (input$base_plot_trigger == "histogram") {
            width <- binwidth()

            if (width > 0) {
                plot <-
                    DrawHistogram(
                        data = series(),
                        binwidth = width,
                        fit = input$dnorm
                    )
            } else {
                plot <- DrawHistogram(data = series(), fit = input$dnorm)
            }

            if (input$density) {
                plot <-
                    plot + geom_density(colour = "#999999",
                                        fill = "#009E73",
                                        alpha = .5)
            }
        } else {
            plot <- DrawQuantileQuantile(data = series()$temperature)
        }

        plot
    })

    output$rule <- renderText({
        rule <- switch(input$rule,
                       sturges = sturges,
                       scott = scott,
                       fd = fd)

        format(rule(), digits = 3)
    })

    output$dstats <- renderDataTable({
        dstats.describe(series()$temperature,
                        locale = TRUE,
                        shiny = TRUE)
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    output$normality <- renderUI({
        test <- switch(
            input$ntest,
            shapiro = ntest.ShapiroWilk,
            pearson = ntest.PearsonChi2,
            ks = ntest.KolmogorovSmirnov
        )

        result <- test(data = series()$temperature)
        statistic <-
            paste("<b>Статистика:</b>", format(result$statistic[[1]]))
        p.value <-
            paste("<b>P-значение:</b>", format(result$p.value))
        conclusion <-
            paste(
                "<b>Заключение:</b>",
                ifelse(
                    result$p.value <= .05,
                    "Нулевая гипотеза о нормальном распределении данных отклонена.",
                    "Нельзя отвергнуть нулевую гипотезу о нормальном распределении данных."
                )
            )
        HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
    })

    series %>% ggvis( ~ year, ~ temperature) %>%
        layer_points() %>% # could be customizable, e.g. by size, fill
        layer_model_predictions(model = "lm", se = FALSE, stroke := "#0072B2") %>%
        add_axis("x",
                 format = "d",
                 properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
        add_tooltip(function(df)
            paste(df$year, ":", df$temperature)) %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("scatterplot", "scatter_ui")

    output$correlation <- renderText({
        format(cor(series()$temperature, c(1:(
            maxRange() - minRange() + 1
        ))), digits = 5)
    })

    output$ctest <- renderUI({
        result <-
            cor.test(series()$temperature, c(1:(maxRange() - minRange() + 1)), method =
                         "pearson")
        statistic <-
            paste("<b>Статистика:</b>", format(result$statistic[[1]]))
        df <-
            paste("<b>Степеней свободы:</b>",
                  format(result$parameter[["df"]]))
        p.value <-
            paste("<b>P-значение:</b>", format(result$p.value))
        ci <-
            paste(
                "<b>",
                attr(result$conf.int, "conf.level") * 100,
                "% доверительный интервал:</b>",
                "[",
                format(result$conf.int[1], digits = 4),
                ";",
                format(result$conf.int[2], digits = 4),
                "]"
            )
        conclusion <-
            paste(
                "<b>Заключение:</b>",
                ifelse(
                    result$p.value <= .05,
                    "Нулевая гипотеза (коэффициент корреляции равен 0) отклонена.",
                    "Нельзя отвергнуть нулевую гипотезу о равенстве 0 коэффициента корреляции."
                )
            )
        HTML(paste(statistic, p.value, df, ci, conclusion, sep = '<br/>'))
    })

    linear <- function(x, a, b)
        a * x + b
    line <- reactive({
        data.frame(x_rng = breaks(),
                   y_rng = sapply(
                       c((minRange()):(maxRange())),
                       FUN = linear,
                       a = coef(model())[2],
                       b = coef(model())[1]
                   ))
    })

    mix <- reactive({
        if (input$residuals_trigger == "src") {
            data.frame(series(), line())
        } else {
            data.frame(
                residuals(),
                x_rng = breaks(),
                y_rng = rep(0, maxRange() - minRange() + 1)
            )
        }
    })

    mix %>% ggvis(x =  ~ year, y =  ~ temperature) %>%
        layer_paths(x = ~ x_rng, y = ~ y_rng, stroke := "blue") %>% # add checkbox to show or not
        layer_points(x =  ~ year, y =  ~ temperature) %>% layer_paths() %>%
        add_axis(
            "x",
            title = "Год наблюдения",
            format = "d",
            properties = axis_props(labels = list(angle = 45, align = "left"))
        ) %>%
        add_axis("y", title = "Температура, ºС") %>%
        add_tooltip(function(df)
            paste(df$year, ":", df$temperature)) %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("regression", "regression_ui")

    output$lm <- renderUI({
        m <- model()
        HTML(sprintf("y = %.03ft + %.03f", coef(model())[2], coef(model())[1]))
        #     withMathJax(
        #       helpText(paste("$$ y = ", format(coef(model())[2], digits=4), "x +", format(coef(model())[2], digits=4), "$$"))
        #     )
    })

    output$signif <- renderUI({
        result <- regr.significance(series()$temperature, math = '')
        valueA <-
            paste("<b>Значение:</b>", format(result$coeff[[1]]))
        statisticA <-
            paste("<b>Статистика Стьюдента:</b>",
                  format(result$statistic[[1]]))
        conclusionA <-
            paste("<b>Заключение:</b>", result$conclusion[[1]])
        valueB <-
            paste("<b>Значение:</b>", format(result$coeff[[2]]))
        statisticB <-
            paste("<b>Статистика Стьюдента:</b>",
                  format(result$statistic[[2]]))
        conclusionB <-
            paste("<b>Заключение:</b>", result$conclusion[[2]])
        critical <-
            paste("<b>Критическое значение:</b>",
                  format(result$critical))
        HTML(paste(
            "<h5>Коэффициент a</h5>",
            paste(valueA, statisticA, critical, conclusionA, sep = "<br>"),
            "<h5>Коэффициент b</h5>",
            paste(valueB, statisticB, critical, conclusionB, sep = '<br/>')
        ))
    })

    output$adequacy <- renderUI({
        result <- regr.adequacy(series()$temperature)
        determ <-
            paste("<b>Коэффициент детерминации:</b>",
                  format(result$determination))
        linearity <-
            paste(
                "<b>Линейность:</b>",
                ifelse(
                    result$linearity < .1,
                    "Присутствует незначительное отклонение от линейности",
                    "Присутствует отклонениение от линейности"
                )
            )
        statistic <-
            paste("<b>Статистика:</b>",
                  format(result$Fisher$statistic))
        critical <-
            paste("<b>Критическое значение:</b>",
                  format(result$Fisher$critical))
        conclusion <-
            paste("<b>Заключение:</b>", result$Fisher$conclusion)
        HTML(paste(
            "<h5>Линейность</h5>",
            paste(determ, linearity, sep = "<br>"),
            "<h5>Критерий Фишера</h5>",
            paste(statistic, critical, conclusion, sep = '<br/>')
        ))
    })

    observe({
        inp <- input$range
        n <- input$range[2] - input$range[1]
        cutoff <- trunc(2 * n / 3)
        updateSliderInput(
            session,
            "residual_range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateSliderInput(
            session,
            "variogram_range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateNumericInput(
            session,
            "cutoff",
            value = cutoff,
            min = 1,
            max = n,
            step = 1
        )
    })

    observe({
        inp <- input$residual_range
        n <- input$residual_range[2] - input$residual_range[1]
        cutoff <- trunc(2 * n / 3)
        updateSliderInput(
            session,
            "range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateSliderInput(
            session,
            "variogram_range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateNumericInput(
            session,
            "cutoff",
            value = cutoff,
            min = 1,
            max = n,
            step = 1
        )
    })

    observe({
        inp <- input$variogram_range
        n <- input$variogram_range[2] - input$variogram_range[1]
        cutoff <- trunc(2 * n / 3)
        updateSliderInput(
            session,
            "range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateSliderInput(
            session,
            "residual_range",
            value = inp,
            min = 1,
            max = nrows,
            step = 1
        )
        updateNumericInput(
            session,
            "cutoff",
            value = cutoff,
            min = 1,
            max = n,
            step = 1
        )
    })

    output$residual_source <- renderDataTable({
        df <- residuals()
        colnames(df) <- c("Год наблюдения", "Температура")
        df
    })

    residuals %>% ggvis( ~ year, ~ temperature) %>% layer_points() %>% layer_paths() %>%
        add_axis(
            "x",
            title = "Год наблюдения",
            format = "d",
            properties = axis_props(labels = list(angle = 45, align = "left"))
        ) %>%
        add_axis("y", title = "Температура, ºС", title_offset = 60) %>%
        add_tooltip(function(df)
            paste(df$year, ":", df$temperature)) %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("residual_overview", "overview_ui")

    residual_binwidth <- reactive({
        input$residual_binwidth
    })

    output$residual_plot <- renderPlot({
        plot <- NA

        if (input$residual_base_plot_trigger == "histogram") {
            width <- residual_binwidth()

            if (width > 0) {
                plot <-
                    DrawHistogram(
                        data = residuals(),
                        binwidth = width,
                        fit = input$residual_dnorm
                    )
            } else {
                plot <- DrawHistogram(data = residuals(),
                                      fit = input$residual_dnorm)
            }

            if (input$residual_density) {
                plot <-
                    plot + geom_density(colour = "#999999",
                                        fill = "#009E73",
                                        alpha = .5)
            }
        } else {
            plot <- DrawQuantileQuantile(data = residuals()$temperature)
        }

        plot
    })

    output$residual_rule <- renderText({
        rule <- switch(
            input$residual_rule,
            sturges = sturges,
            scott = scott,
            fd = fd
        )

        format(rule(), digits = 3)
    })

    output$residual_dstats <- renderDataTable({
        dstats.describe(residuals()$temperature,
                        locale = TRUE,
                        shiny = TRUE)
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    output$residual_normality <- renderUI({
        test <- switch(
            input$residual_ntest,
            shapiro = ntest.ShapiroWilk,
            pearson = ntest.PearsonChi2,
            ks = ntest.KolmogorovSmirnov
        )
        result <- test(data = residuals()$temperature)
        statistic <-
            paste("<b>Статистика:</b>", format(result$statistic[[1]]))
        p.value <-
            paste("<b>P-значение:</b>", format(result$p.value))
        conclusion <-
            paste(
                "<b>Заключение:</b>",
                ifelse(
                    result$p.value <= .05,
                    "Нулевая гипотеза отклонена.",
                    "Нельзя отвергнуть нулевую гипотезу."
                )
            )
        HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
    })

    output$acf <- renderPlot({
        DrawAutoCorrelationFunction(residuals()$temperature)
    })

    output$ljung <- renderUI({
        result <- Box.test(residuals()$temperature, type = "Ljung-Box")
        statistic <-
            paste("<b>Статистика:</b>", format(result$statistic[[1]]))
        p.value <-
            paste("<b>P-значение:</b>", format(result$p.value))
        conclusion <-
            paste(
                "<b>Заключение:</b>",
                ifelse(
                    result$p.value <= .05,
                    "Нулевая гипотеза (данные являются случайными) отклонена.",
                    "Нельзя отвергнуть нулевую гипотезу о случайности данных."
                )
            )
        HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
    })

    output$adf <- renderUI({
        result <- adf.test(residuals()$temperature)
        statistic <-
            paste("<b>Статистика:</b>", format(result$statistic[[1]]))
        p.value <-
            paste("<b>P-значение:</b>", format(result$p.value))
        conclusion <-
            paste(
                "<b>Заключение:</b>",
                ifelse(
                    result$p.value <= .05,
                    "Нулевая гипотеза о стационарности данных отклонена.",
                    "Нельзя отвергнуть нулевую гипотезу о стационарности данных."
                )
            )
        HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
    })

    modelV <- reactive({
        input$modelV
    })

    cutoff <- reactive({
        r <- input$cutoff
        if (!is.na(r)) {
            r
        } else {
            1
        }
    })

    nugget <- reactive({
        r <- input$nugget
        if (!is.na(r)) {
            r
        } else {
            .1
        }
    })

    range <- reactive({
        r <- input$rangeV
        if (!is.na(r)) {
            r
        } else {
            .1
        }
    })

    psill <- reactive({
        r <- input$psill
        if (!is.na(r)) {
            r
        } else {
            0
        }
    })

    basicVariogram <- reactive({
        observations <- maxRange() - minRange() + 1
        data <- residuals()$temperature
        spdata <-
            data.frame("x" = c(1:observations),
                       "y" = rep(1, observations))
        coordinates(spdata) =  ~ x + y

        if (!input$afv) {
            exp_var <-
                variogram(
                    data ~ 1,
                    spdata,
                    width = 1,
                    cutoff = cutoff(),
                    cressie = input$cressie
                )
            if (psill() == 0) {
                var_model <- vgm(
                    model = modelV(),
                    range = range(),
                    nugget = nugget()
                )
            } else {
                var_model <-
                    vgm(
                        model = modelV(),
                        psill = psill(),
                        range = range(),
                        nugget = nugget()
                    )
            }
            if (input$fitVariogram) {
                var_model <-
                    fit.variogram(exp_var,
                                  var_model,
                                  debug.level = 0,
                                  fit.method = 6)
            }

            variogram <-
                list(
                    exp_var = exp_var,
                    var_model = var_model,
                    sserr = ifelse(is.null(attr(
                        var_model, "SSErr"
                    )), "", attr(var_model, "SSErr"))
                )

        } else {
            variogram <-
                autofitVariogram(data ~ 1,
                                 spdata,
                                 cutoff = cutoff(),
                                 cressie = input$cressie)
        }
    })

    fittedVariogram <- reactive({
        # Arrange the data for the ggplot2 plot
        # add the semivariance values of v2 to v1
        Fitted <-
            data.frame(dist = seq(0.01, max(basicVariogram()$exp_var$dist), length = cutoff()))
        Fitted$gamma <-
            variogramLine(basicVariogram()$var_model, dist_vector = Fitted$dist)$gamma
        #convert the dataframes to a long format
        Empirical <-
            melt(
                basicVariogram()$exp_var,
                id.vars = "dist",
                measure.vars = c("gamma")
            )
        Modeled <-
            melt(Fitted,
                 id.vars = "dist",
                 measure.vars = c("gamma"))

        data.frame(Empirical,
                   x_rng = Modeled$dist,
                   y_rng = Modeled$value)
    })

    fittedVariogram %>% ggvis(x =  ~ dist, y =  ~ value) %>%
        layer_paths(x =  ~ x_rng, y =  ~ y_rng, stroke := "blue") %>% # add checkbox to show or not
        layer_points(x =  ~ dist, y =  ~ value) %>%
        add_axis("x", title = "Лаг", format = "d") %>%
        add_axis("y", title = "Семивариограмма") %>%
        add_tooltip(function(df)
            paste(
                paste("<b>Лаг:</b>", df$dist),
                paste("<b>Значение:</b>", df$value),
                sep = "<br>"
            )) %>%
        set_options(width = "auto") %>%
        bind_shiny("variogram", "variogram_ui")

    output$text_model <- renderUI({
        getModelName <- function (model) {
            switch(
                as.character(model$model),
                "Nug" = "Эффект самородков",
                "Exp" = "Экспоненциальная модель",
                "Sph" = "Сферическая модель",
                "Gau" = "Гауссовская модель",
                "Mat" = "Модель Матерна",
                "Ste" = "Модель Штейна",
                "Cir" = "Круговая модель",
                "Lin" = "Линейная модель",
                "Bes" = "Модель Бесселя",
                "Pen" = "Пентасферическая модель",
                "Per" = "Периодическая модель",
                "Wav" = "Волновая модель",
                "Hol" = "Модель с эффектом дыр",
                "Log" = "Логарифмическая модель",
                "Spl" = "Сплайн-модель",
                "Leg" = "Модель Лежандра"
            )
        }

        models <- basicVariogram()$var_model
        text <- ""
        for (i in 1:length(models[, 1])) {
            model <- models[i,]
            if (model$model == "Nug") {
                textModel <-
                    paste0("<b>",
                           getModelName(model),
                           ": </b>",
                           format(model$psill))
            } else {
                textModel <-
                    paste(
                        "<b>",
                        getModelName(model),
                        "</b>",
                        "<br><b>Ранг:</b>",
                        format(model$range),
                        "<br><b>Порог:</b>",
                        format(model$psill)
                    )
            }
            text <-
                paste(text, textModel, sep = ifelse(i == 1, "", "<hr>"))
        }
        HTML(text)
    })

    output$sserr <- renderUI({
        HTML(ifelse(
            input$afv ||
                input$fitVariogram,
            paste("<b>Невязка:</b>", format(basicVariogram()$sserr)),
            ""
        ))
    })

    observations <- reactive({
        maxRange() - minRange() + 1
    })

    future <- reactive({
        f <- input$future
        if (is.na(f)) {
            0
        } else {
            f
        }
    })

    kriging <- reactive({
        PredictWithKriging(
            residuals()$temperature,
            x = c(1:observations()),
            observations = observations(),
            variogram_model = basicVariogram()$var_model,
            nrows = nrows,
            future = future()
        )
    })

    output$predictions <- renderDataTable({
        k <- kriging()
        data.frame(
            "Прогноз" = sapply(k$var1.pred, signif, digits = 4),
            "Среднеквадратическое отклонение" = sapply(sapply(k$var1.var, sqrt), signif, digits =
                                                           4)
        )
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    output$analysis <- renderDataTable({
        obs <- observations()
        k <- kriging()
        data.frame(
            "Год" = src$year[(obs + 1):nrows],
            "Наблюдение" = src$temperature[(obs + 1):nrows],
            "Тренд" = sapply(trend()[(obs + 1):nrows], signif, digits =
                                 4),
            "Прогноз" = sapply(k$var1.pred + trend()[(obs + 1):nrows], signif, digits =
                                   4),
            "Остаток" = sapply(
                src$temperature[(obs + 1):nrows] - (k$var1.pred + trend()[(obs + 1):nrows]),
                signif,
                digits = 4
            )
        )
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    computeTrend <- function (fit, future = 0) {
        c(sapply(
            c(1:(nrows + future)),
            FUN = function(x)
                fit$coefficients[[1]] + x * fit$coefficients[[2]]
        ))
    }

    trend <- reactive({
        computeTrend(model(), future())
    })

    cp <- reactive({
        future <- future()
        obs <- observations()
        year = GetPredictionYears(src$year, nrows, future, obs)

        actual <-
            c(src$temperature[(obs - 1):nrows], rep(src$temperature[nrows], future))
        prediction.trend <-
            c(src$temperature[(obs - 1):obs], trend()[(obs + 1):(nrows + future)])
        prediction.kriging <-
            c(src$temperature[(obs - 1):obs], trend()[(obs + 1):(nrows + future)] + kriging()$var1.pred)

        a <-
            melt(
                data.frame(
                    year,
                    "Наблюдение" = actual,
                    "Тренд" = prediction.trend,
                    "Кригинг" = prediction.kriging
                ),
                id = c("year")
            )
    })

    cp %>% ggvis(x =  ~ year,
                 y =  ~ value,
                 stroke =  ~ variable) %>%
        layer_points(fill := "white", size := 20) %>%
        layer_lines(strokeDash =  ~ variable) %>%
        add_axis("x", title = "Год наблюдения", format = "d") %>%
        add_axis("y", title = "Температура, ºС") %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("cross_prediction", "cross_prediction_ui")

    #output$cross_prediction <- renderPlot({
    #  obj <- cp()
    #  DrawCrossPrediction(actual=obj$actual, trend=obj$trend, kriging=obj$kriging, future=0)
    #})

    measure <- reactive({
        if (input$cross) {
            input$cvm
        } else {
            switch(
                input$measure,
                MAE  = MAE,
                MSE  = MSE,
                RMSE = RMSE
            )
        }
    })

    measureText <- reactive({
        if (input$cross) {
            switch(
                input$cvm,
                MAE  = "MAE",
                MSE  = "MSE",
                RMSE = "RMSE",
                RSS  = "RSS",
                cor_obspred = "Корреляция"
            )
        } else {
            input$measure
        }
    })

    computePredictionEstimation <-
        function(data,
                 trend,
                 variog = ComputeVariogram,
                 cressie,
                 x,
                 cutoff) {
            variogram <-
                variog(
                    data,
                    x = x,
                    cressie = cressie,
                    cutoff = cutoff,
                    observations = observations()
                )
            if (!input$cross) {
                kriging <-
                    PredictWithKriging(
                        residuals()$temperature,
                        x = x,
                        observations = observations(),
                        variogram_model = variogram$var_model,
                        nrows = nrows
                    )
                residual <-
                    ComputeKrigingResiduals(src$temperature,
                                            trend,
                                            kriging,
                                            observations(),
                                            nrows)
                estimation <- measure()(residual)
            } else {
                crv <-
                    computeCV(residuals()$temperature,
                              variogram$var_model,
                              observations(),
                              nfold())
                estimation <- compStat(crv)[[measure()]]
            }
            return(estimation)
        }

    computeComparison <- eventReactive(input$computeComparison, {
        # Create 0-row data frame which will be used to store data
        dat <-
            data.frame(
                cutoff = numeric(0),
                "Матерона" = numeric(0),
                "Кресси-Хокинса" = numeric(0)
            )

        withProgress(message = 'Идет вычисление', value = 0, {
            # Number of times we'll go through the loop
            n <- maxRange()
            data <- residuals()$temperature
            trend <- trend()
            cutoffs <- 1:n

            for (cutoff in cutoffs) {
                classicalResult <-
                    computePredictionEstimation(
                        data = data,
                        trend = trend,
                        x = c(1:observations()),
                        cressie = FALSE,
                        cutoff = cutoff
                    )
                robustResult    <-
                    computePredictionEstimation(
                        data = data,
                        trend = trend,
                        x = c(1:observations()),
                        cressie = TRUE,
                        cutoff = cutoff
                    )

                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                dat <-
                    rbind(
                        dat,
                        data.frame(
                            cutoff = cutoff,
                            "Матерона" = classicalResult,
                            "КрессиХокинса" = robustResult
                        )
                    )

                # Increment the progress bar, and update the detail text.
                incProgress(1 / n, detail = paste0(trunc(cutoff / n * 100), "%"))
            }
        })
        dat
    })

    output$param_comparison <- renderPlot({
        dat <- melt(computeComparison(), id = c("cutoff"))
        ggplot(
            data = dat,
            aes(
                x = cutoff,
                y = value,
                group = variable,
                color = variable,
                linetype = variable
            )
        ) +
            geom_line() +
            # geom_point(size=2, shape=21, fill="white") +
            scale_x_continuous(breaks = 1:maxRange()) +
            xlab("Максимальное расстояние") + ylab(measureText()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })

    output$best_cutoff <- renderDataTable({
        cmp <- computeComparison()
        df <-
            data.frame(c("Матерона", "Кресси-Хокинса"),
                       c(
                           which.min(cmp$Матерона),
                           which.min(cmp$Кресси-Хокинса)
                       ),
                       c(min(cmp$Матерона), min(cmp$КрессиХокинса)))
        colnames(df) <- c("Оценка", "Расстояние", measureText())
        rownames(df) <- c("Матерона", "Кресси-Хокинса")

        df
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    fitmin <- reactive({
        r <- input$fit_min
        if (!is.na(r)) {
            r
        } else {
            0
        }
    })

    fitmax <- reactive({
        r <- input$fit_max
        if (!is.na(r)) {
            r
        } else {
            0
        }
    })

    fitstep <- reactive({
        r <- input$fit_step
        if (!is.na(r)) {
            r
        } else {
            .1
        }
    })

    fitVariogram <- reactive({
        input$fitVariogram
    })

    computeEstimation <-
        function(data,
                 trend,
                 cressie,
                 x,
                 cutoff,
                 observations,
                 fit,
                 model,
                 nugget,
                 sill,
                 range) {
            variogram <-
                ComputeManualVariogram(
                    data,
                    x = x,
                    cressie = cressie,
                    cutoff = cutoff,
                    observations = observations,
                    fit = fit,
                    model = model,
                    nugget = nugget,
                    psill = sill,
                    range = range
                )
            if (!input$cross) {
                kriging <-
                    PredictWithKriging(
                        data,
                        x = x,
                        observations = observations,
                        variogram_model = variogram$var_model,
                        nrows = nrows
                    )
                residual <-
                    ComputeKrigingResiduals(src$temperature,
                                            trend,
                                            kriging,
                                            observations,
                                            nrows)
                estimation <- measure()(residual)
            } else {
                crv <-
                    computeCV(residuals()$temperature,
                              variogram$var_model,
                              observations(),
                              nfold())
                estimation <- compStat(crv)[[measure()]]
            }

            return(estimation)
        }

    fitParameter <- eventReactive(input$fitParam, {
        isCutoff <- input$fit_param == 1
        isNugget <- input$fit_param == 2
        isSill   <- input$fit_param == 3
        isRange  <- input$fit_param == 4

        params <- seq(fitmin(), fitmax(), fitstep())
        if (isCutoff) {
            params <- 1:observations()
            caption <- "Максимальное расстояние"
        } else if (isNugget) {
            caption <- "Наггет"
        } else if (isSill) {
            caption <- "Порог"
        } else if (isRange) {
            caption <- "Ранг"
        }
        result <- numeric(0)

        data <- residuals()$temperature
        trend <- trend()

        withProgress(message = 'Идет вычисление', value = 0, {
            n <- length(params)
            i <- 0

            for (param in params) {
                i <- i + 1
                result <-
                    c(
                        result,
                        computeEstimation(
                            data = data,
                            trend = trend,
                            x = c(1:observations()),
                            cressie = input$cressie,
                            observations = observations(),
                            fit = fitVariogram(),
                            model = modelV(),
                            cutoff = ifelse(isCutoff, param, cutoff()),
                            nugget = ifelse(isNugget, param, nugget()),
                            sill = ifelse(isSill, param, psill()),
                            range = ifelse(isRange, param, range())
                        )
                    )

                # Increment the progress bar, and update the detail text.
                incProgress(1 / n, detail = paste0(trunc(i / n * 100), "%"))
            }
        })
        list(
            result = result,
            params = params,
            caption = caption,
            wmin = params[which.min(result)],
            wmax = params[which.max(result)],
            min = format(min(result), digits = 3),
            max = format(max(result), digits = 3)
        )
    })

    output$fit_param <- renderPlot({
        obj <- fitParameter()

        ggplot(data = data.frame("X" = obj$params, "Y" = obj$result), aes(x =
                                                                              X, y = Y)) +
            geom_line() +
            scale_x_continuous(breaks = obj$params[seq(1, length(obj$params), 4)]) +
            xlab(paste0(obj$caption, ", min=", obj$wmin, ", max=", obj$wmax)) + ylab(paste0(measureText(), ", min=", obj$min, ", max=", obj$max)) +
            theme(
                axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title = element_text(size = 15)
            )
    })
    nfold <- reactive({
        input$nfold
    })
    cv <- reactive({
        computeCV(
            residuals()$temperature,
            basicVariogram()$var_model,
            observations(),
            nfold()
        )
    })
    sig <- function(vec) {
        sapply(vec, signif, digits = 4)
    }
    output$cv <- renderDataTable({
        obj <- cv()
        data.frame(
            "Прогноз" = sig(obj$var1.pred),
            "Среднеквадратическое отклонение" = sig(sapply(obj$var1.var, sqrt)),
            "Наблюдение" = sig(obj$observed),
            "Остаток" = sig(obj$residual),
            "Zзначение" = sig(obj$zscore)
        )
    },  options = list(searching = FALSE))

    output$cv_stats <- renderDataTable({
        computeCVStatistics(cv())
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    output$cv_stats2 <- renderDataTable({
        computeCVStatistics(cv())
    }, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
    ))

    cv_err <- reactive({
        obj <- cv()
        relativeError <- obj$residual / obj$observed
        #relativeError <- obj$var1.pred[1:maxRange()] + trend()
        data.frame(error = relativeError, year = series()$year)
    })

    cv_err %>% ggvis(x =  ~ year, y =  ~ error) %>%
        layer_points(fill := "white", size := 20, stroke := "black") %>%
        layer_lines() %>%
        add_axis("x", title = "Год наблюдения", format = "d") %>%
        add_axis("y", title = "Ошибка") %>%
        add_tooltip(function(df)
            paste(df$year, ":", format(df$error, digits = 3))) %>%
        scale_numeric("x", nice = FALSE) %>%
        set_options(width = "auto") %>%
        bind_shiny("cv_plot", "cv_plot_ui")
})
