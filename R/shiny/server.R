library(shiny)
library(ggplot2)  # eye-candy graphs
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
source("lib/kriging.R")

## Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

kMinYear <- min(src.data$year)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  minYear <- reactive({
    if (input$nav == "Анализ остатков") {
      input$residual_range[1]
    } else {
      input$range[1]
    }
  })
  maxYear <- reactive({
    if (input$nav == "Анализ остатков") {
      input$residual_range[2]
    } else {
      input$range[2]
    }
  })
  series <- reactive({
    src.data[minYear():maxYear(),]
  })
  breaks <- reactive({src.data[minYear():maxYear(),1]})
  binwidth <- reactive({input$binwidth})
  sturges <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.Sturges(data)
  })
  scott <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.FD(data)
  })
  fd <- reactive({
    data <- series()$temperature
    (max(data)-min(data)) / nclass.scott(data)
  })
  datebreaks <- reactive({
    seq(kMinYear - 5 + minYear(), kMinYear + 5 + maxYear(), by=2)
  })
  model <- reactive({
    lm(series()$temperature ~ c(minYear():maxYear()))
  })
  residuals <- reactive({
    data.frame(year = series()$year, temperature=model()$residuals)
  })
  
  output$datasource <- renderDataTable({
    df <- series()
    colnames(df) <- c("Год наблюдения", "Температура")
    df
  })

  series %>% ggvis(~year, ~temperature) %>% layer_points() %>% layer_paths() %>% 
    add_axis("x", title="Год наблюдения", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_axis("y", title="Температура, ºС") %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("overview", "overview_ui")
  
  output$base_plot <- renderPlot({
    plot <- NA
    
    if (input$base_plot_trigger == "histogram") {
      width <- binwidth()
      
      if (width > 0) {
        plot <- DrawHistogram(data=series(), binwidth=width, fit=input$dnorm)
      } else {
        plot <- DrawHistogram(data=series(), fit=input$dnorm)
      }
      
      if (input$density) {
        plot <- plot + geom_density(colour="#999999", fill="#009E73", alpha=.5)
      } 
    } else {
      plot <- DrawQuantileQuantile(data=series()$temperature)
    }
    
    plot
  })
  
  output$rule <- renderText({
    rule <- switch(input$rule,
                   sturges = sturges,
                   scott = scott,
                   fd = fd)
    
    format(rule(), digits=3)
  })
  
  output$dstats <- renderTable({
    dstats.describe(series()$temperature, locale=TRUE)
  })
  
  output$normality <- renderUI({
    test <- switch(input$ntest,
                   shapiro = ntest.ShapiroWilk,
                   pearson = ntest.PearsonChi2,
                   ks = ntest.KolmogorovSmirnov)
    
    result <- test(data=series()$temperature)
    statistic <- paste("<b>Статистика:</b>", format(result$statistic[[1]]))
    p.value <- paste("<b>P-значение:</b>", format(result$p.value))
    conclusion <- paste("<b>Заключение:</b>", ifelse(result$p.value <= .05, "Нулевая гипотеза отклонена.", "Нельзя отвергнуть нулевую гипотезу."))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  series %>% ggvis(~year, ~temperature) %>% 
    layer_points() %>% # could be customizable, e.g. by size, fill
    layer_model_predictions(model="lm", se=FALSE, stroke := "#0072B2") %>%
    add_axis("x", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("scatterplot", "scatter_ui")
  
  output$correlation <- renderText({
    format(cor(series()$temperature, c(1:(maxYear() - minYear() + 1))), digits=5)
  })
  
  output$ctest <- renderUI({    
    result <- cor.test(series()$temperature, c(1:(maxYear() - minYear() + 1)), method="pearson")
    statistic <- paste("<b>Статистика:</b>", format(result$statistic[[1]]))
    df <- paste("<b>Степеней свободы:</b>", format(result$parameter[["df"]]))
    p.value <- paste("<b>P-значение:</b>", format(result$p.value))
    ci <- paste("<b>", attr(result$conf.int, "conf.level") * 100, "% доверительный интервал:</b>", "[", format(result$conf.int[1], digits=4), ";", format(result$conf.int[2], digits=4), "]")
    conclusion <- paste("<b>Заключение:</b>", ifelse(result$p.value <= .05, "Нулевая гипотеза (коэффициент корреляции равен 0) отклонена.", "Нельзя отвергнуть нулевую гипотезу о равенстве 0 коэффициента корреляции."))
    HTML(paste(statistic, p.value, df, ci, conclusion, sep = '<br/>'))
  })
  
  linear <- function(x, a, b) a * x + b
  line <- reactive({
    data.frame(
      x_rng = breaks(), 
      y_rng = sapply(c((minYear()):(maxYear())), FUN=linear, a=coef(model())[2], b=coef(model())[1])
    )
  })
  
  mix <- reactive({
    if (input$residuals_trigger == "src") {
      data.frame(series(), line())
    } else {
      data.frame(residuals(), x_rng=breaks(), y_rng=rep(0, maxYear() - minYear() + 1))
    }
  })
  
  mix %>% ggvis(x=~year, y=~temperature) %>%
    layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue") %>% # add checkbox to show or not
    layer_points(x=~year, y=~temperature) %>% layer_paths() %>% 
    add_axis("x", title="Год наблюдения", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_axis("y", title="Температура, ºС") %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("regression", "regression_ui")
  
  output$lm <- renderUI({
    m <- model()
    withMathJax(sprintf("$$ y = %.03f t + %.03f $$", coef(model())[2], coef(model())[1]))
#     withMathJax(
#       helpText(paste("$$ y = ", format(coef(model())[2], digits=4), "x +", format(coef(model())[2], digits=4), "$$"))  
#     )
  })
  
  output$signif <- renderUI({    
    result <- regr.significance(series()$temperature, math='')
    valueA <- paste("<b>Значение:</b>", format(result$coeff[[1]]))
    statisticA <- paste("<b>Статистика Стьюдента:</b>", format(result$statistic[[1]]))
    conclusionA <- paste("<b>Заключение:</b>", result$conclusion[[1]])
    valueB <- paste("<b>Значение:</b>", format(result$coeff[[2]]))
    statisticB <- paste("<b>Статистика Стьюдента:</b>", format(result$statistic[[2]]))
    conclusionB <- paste("<b>Заключение:</b>", result$conclusion[[2]])
    critical <- paste("<b>Критическое значение:</b>", format(result$critical))
    HTML(paste("<h5>Коэффициент a</h5>", paste(valueA, statisticA, critical, conclusionA, sep="<br>"), "<h5>Коэффициент b</h5>", paste(valueB, statisticB, critical, conclusionB, sep = '<br/>')))
  })
  
  output$adequacy <- renderUI({    
    result <- regr.adequacy(series()$temperature)
    determ <- paste("<b>Коэффициент детерминации:</b>", format(result$determination))
    linearity <- paste("<b>Линейность:</b>", ifelse(result$linearity < .1, "Присутствует незначительное отклонение от линейности", "Присутствует отклонениение от линейности"))
    statistic <- paste("<b>Статистика:</b>", format(result$Fisher$statistic))
    critical <- paste("<b>Критическое значение:</b>", format(result$Fisher$critical))
    conclusion <- paste("<b>Заключение:</b>", result$Fisher$conclusion)
    HTML(paste("<h5>Линейность</h5>", paste(determ, linearity, sep="<br>"), "<h5>Критерий Фишера</h5>", paste(statistic, critical, conclusion, sep = '<br/>')))
  })

  observe({    
    inp <- input$range
    updateSliderInput(session, "residual_range", value=inp, min=1, max=38, step=1)
  })
  
  observe({    
    inp <- input$residual_range
    updateSliderInput(session, "range", value=inp, min=1, max=38, step=1)
  })
  
  output$residual_source <- renderDataTable({
    df <- residuals()
    colnames(df) <- c("Год наблюдения", "Температура")
    df
  })
  
  residuals %>% ggvis(~year, ~temperature) %>% layer_points() %>% layer_paths() %>% 
    add_axis("x", title="Год наблюдения", format="d", properties=axis_props(labels=list(angle=45, align="left"))) %>%
    add_axis("y", title="Температура, ºС", title_offset=60) %>%
    add_tooltip(function(df) paste(df$year, ":", df$temperature)) %>%
    scale_numeric("x", nice = FALSE) %>%
    bind_shiny("residual_overview", "overview_ui")
  
  residual_binwidth <- reactive({input$residual_binwidth})
  
  output$residual_plot <- renderPlot({
    plot <- NA
    
    if (input$residual_base_plot_trigger == "histogram") {
      width <- residual_binwidth()
      
      if (width > 0) {
        plot <- DrawHistogram(data=residuals(), binwidth=width, fit=input$residual_dnorm)
      } else {
        plot <- DrawHistogram(data=residuals(), fit=input$residual_dnorm)
      }
      
      if (input$residual_density) {
        plot <- plot + geom_density(colour="#999999", fill="#009E73", alpha=.5)
      } 
    } else {
      plot <- DrawQuantileQuantile(data=residuals()$temperature)
    }
    
    plot
  })
  
  output$residual_rule <- renderText({
    rule <- switch(input$residual_rule,
      sturges = sturges,
      scott = scott,
      fd = fd)
    
    format(rule(), digits=3)
  })
  
  output$residual_dstats <- renderTable({
    dstats.describe(residuals()$temperature, locale=TRUE)
  })
  
  output$residual_normality <- renderUI({
    test <- switch(input$residual_ntest,
      shapiro = ntest.ShapiroWilk,
      pearson = ntest.PearsonChi2,
      ks = ntest.KolmogorovSmirnov)
    result <- test(data=residuals()$temperature)
    statistic <- paste("<b>Статистика:</b>", format(result$statistic[[1]]))
    p.value <- paste("<b>P-значение:</b>", format(result$p.value))
    conclusion <- paste("<b>Заключение:</b>", ifelse(result$p.value <= .05, "Нулевая гипотеза отклонена.", "Нельзя отвергнуть нулевую гипотезу."))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  output$acf <- renderPlot({
    DrawAutoCorrelationFunction(residuals()$temperature)
  })
  
  output$ljung <- renderUI({
    result <- Box.test(residuals()$temperature, type="Ljung-Box")
    statistic <- paste("<b>Статистика:</b>", format(result$statistic[[1]]))
    p.value <- paste("<b>P-значение:</b>", format(result$p.value))
    conclusion <- paste("<b>Заключение:</b>", ifelse(result$p.value <= .05, "Нулевая гипотеза (данные являются случайными) отклонена.", "Нельзя отвергнуть нулевую гипотезу о случайности данных."))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  output$adf <- renderUI({
    result <- adf.test(residuals()$temperature)
    statistic <- paste("<b>Статистика:</b>", format(result$statistic[[1]]))
    p.value <- paste("<b>P-значение:</b>", format(result$p.value))
    conclusion <- paste("<b>Заключение:</b>", ifelse(result$p.value <= .05, "Нулевая гипотеза о стационарности данных отклонена.", "Нельзя отвергнуть нулевую гипотезу о стационарности данных."))
    HTML(paste(statistic, p.value, conclusion, sep = '<br/>'))
  })
  
  modelV <- reactive({
    input$modelV
  })
  
  cutoff <- reactive({
    input$cutoff
  })
  
  nugget <- reactive({
    input$nugget
  })
  
  range <- reactive({
    r <- input$rangeV
    if(!is.na(r)) {
      r
    } else {
      .1
    }
  })
  
  psill <- reactive({
    input$psill
  })
  
  basicVariogram <- reactive({
    observations <- input$range[2]
    data <- residuals()$temperature
    spdata <- data.frame("x"=c(1:observations), "y"=rep(1, observations))
    coordinates(spdata) =~ x + y
    
    if (!input$afv) {
      exp_var <- variogram(data~1, spdata, width=1, cutoff=cutoff())
      if (psill() == 0) {
        var_model <- vgm(model=modelV(), range=range(), nugget=nugget())  
      } else {
        var_model <- vgm(model=modelV(), psill=psill(), range=range(), nugget=nugget())  
      }     
      if (input$fitVariogram) {
        var_model <- fit.variogram(exp_var, var_model)
      }
      
      variogram <- list(exp_var = exp_var, var_model = var_model, sserr = ifelse(is.null(attr(var_model, "SSErr")), "", attr(var_model, "SSErr")))
      
    } else {
      variogram <- autofitVariogram(data~1, spdata, cutoff=cutoff(), cressie=input$cressie, width=FALSE)
    }
  })
  
  fittedVariogram <- reactive({
    # Arrange the data for the ggplot2 plot
    # add the semivariance values of v2 to v1
    Fitted <- data.frame(dist = seq(0.01, max(basicVariogram()$exp_var$dist), length = cutoff()))
    Fitted$gamma <- variogramLine(basicVariogram()$var_model, dist_vector = Fitted$dist)$gamma
    #convert the dataframes to a long format
    Empirical <- melt(basicVariogram()$exp_var, id.vars = "dist", measure.vars = c("gamma"))
    Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("gamma"))

    data.frame(Empirical, x_rng=Modeled$dist, y_rng=Modeled$value)
  })
  
  fittedVariogram %>% ggvis(x=~dist, y=~value) %>%
    layer_paths(x=~x_rng, y=~y_rng, stroke := "blue") %>% # add checkbox to show or not
    layer_points(x=~dist, y=~value) %>% 
    add_axis("x", title="Лаг", format="d") %>%
    add_axis("y", title="Семивариограмма") %>%
    add_tooltip(function(df) paste(paste("<b>Лаг:</b>", df$dist), paste("<b>Значение:</b>",df$value), sep="<br>")) %>%
    bind_shiny("variogram", "variogram_ui")
  
  output$text_model <- renderUI({
    getModelName <- function (model) {
      switch(as.character(model$model),
        "Nug"="Эффект самородков",
        "Exp"="Экспоненциальная модель",
        "Sph"="Сферическая модель",
        "Gau"="Гауссовская модель",
        "Mat"="Модель Матерна",
        "Ste"="Модель Штейна",
        "Cir"="Круговая модель",
        "Lin"="Линейная модель",
        "Bes"="Модель Бесселя",
        "Pen"="Пентасферическая модель",
        "Per"="Периодическая модель",
        "Wav"="Волновая модель",
        "Hol"="Модель с эффектом дыр",
        "Log"="Логарифмическая модель",
        "Spl"="Сплайн-модель",
        "Leg"="Модель Лежандра")
    }
    
    models <- basicVariogram()$var_model
    text <- ""
    for (i in 1:length(models[, 1])) {
      model <- models[i, ]
      if (model$model == "Nug") {
        textModel <- paste0("<b>", getModelName(model), ": </b>",  format(model$psill))
      } else {
        textModel <- paste("<b>", getModelName(model), "</b>", "<br><b>Ранг:</b>", format(model$range), "<br><b>Порог:</b>", format(model$psill))
      }
      text <- paste(text, textModel, sep=ifelse(i==1, "", "<hr>"))
    }
    HTML(text)
  })
  
  output$sserr <- renderUI({
    HTML(ifelse(input$afv || input$fitVariogram, paste("<b>Невязка:</b>", format(basicVariogram()$sserr)), ""))
  })
  
  kriging <- reactive({
    PredictWithKriging(residuals()$temperature, x=input$range[1]:input$range[2], variogram_model=basicVariogram()$var_model)
  })
  
  output$predictions <- renderTable({
    k <- kriging()
    data.frame("Прогноз"=k$var1.pred, "Дисперсия"=k$var1.var)
  })
})