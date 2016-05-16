handleResiduals <- function (input, output) {
    output$residual_source <- renderDataTable({
        df <- residuals()
        colnames(df) <- c("Год наблюдения", "Температура")
        df
    })

    residuals %>% ggvis(~ year, ~ temperature) %>% layer_points() %>% layer_paths() %>%
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

}
