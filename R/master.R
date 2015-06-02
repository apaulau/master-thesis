## Cleaning up the workspace
rm(list = ls(all = TRUE))

## Dependencies
library(ggplot2)  # eye-candy graphs
library(xtable)   # convert data to latex tables
library(outliers) # tests for outliers
library(tseries)  # adf test used
library(nortest)  # tests for normality
library(sp)       # spatial data
library(gstat)    # geostatistics
library(reshape2) # will see

## Import local modules
source("R/lib/plot.R")       # useful functions for more comfortable plotting
source("R/lib/dstats.R")     # descriptive statistics module
source("R/lib/misc.R")       # some useful global-use functions
source("R/lib/draw.R")       # helpers for drawing
source("R/lib/write.R")      # helpers for writing
source("R/lib/ntest.R")      # tests for normality
source("R/lib/regr.R")
source("R/lib/measures.R")

## Read the data / pattern: year;temperature
path.data <-
  "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
nrows <- 38
src  <-
  read.csv(
    file = path.data, header = TRUE, sep = ";", nrows = nrows, colClasses =
      c("numeric", "numeric"), stringsAsFactors = FALSE
  )

## Global use constants
kDateBreaks <-
  seq(min(src$year) - 5, max(src$year) + 5, by = 2) # date points for graphs

## For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src[, 1]) - 6
WriteCharacteristic(expression = kObservationNum, type = "original", name =
    "n")

## Source data as basic time series plot: points connected with line
plot.source <-
  DrawDataRepresentation(data = src, filename = "source.png", datebreaks =
      kDateBreaks)

tmp <- src
colnames(tmp) <- c("Год", "Температура, ºС")
print(
  xtable(
    tmp, caption = "Исходные данные.", label = "table:source", digits = c(0, 0, 2), align =
      "r|rc|"
  ),  table.placement = "H", caption.placement = 'top',
  file = "out/original/data.tex", include.rownames = FALSE
)

## Form the data for research
sample <- src[0:kObservationNum,]

# Getting descriptive statistics for temperature in russian locale
sample.dstats <-
  dstats.describe(sample$temperature, type = "original", locale = TRUE)
print(
  xtable(sample.dstats, caption = "Описательные статистики для наблюдаемых температур.", label =
      "table:dstats"), caption.placement = 'top',
  file = "out/original/dstats.tex"
)

# Compute Sturges rule for output
WriteCharacteristic(
  expression = nclass.Sturges(sample$temperature), type = "original", name =
    "sturges"
)

## Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.data.hist <-
  DrawHistogram(data = sample, filename = "original/histogram.png")

## Tests for normality
sample.shapiro <-
  ntest.ShapiroWilk(data = sample$temperature, type = "original", name =
      "shapiro")
sample.pearson <-
  ntest.PearsonChi2(data = sample$temperature, type = "original", name =
      "pearson")
sample.ks      <-
  ntest.KolmogorovSmirnov(data = sample$temperature, type = "original", name =
      "ks")

## Normal Quantile-Quantile plot // TODO: check when it appears in text
plot.data.qq <-
  DrawQuantileQuantile(data = sample$temperature, filename = "original/quantile.png")

## Scatter plot with regression line
plot.data.scatter <-
  DrawScatterPlot(sample, filename = "original/scatterplot.png", kDateBreaks);

## Grubbs test for outliers
sample.grubbs <- grubbs.test(sample$temperature)
WriteTest(sample.grubbs$statistic[1], sample.grubbs$p.value, type = "original", name =
    "grubbs")

## Compute correlation for output
sample.correlation <- cor(x = sample$year, y = sample$temperature)
WriteCharacteristic(sample.correlation, type = "original", name = "correlation")

WriteTest(
  sample.correlation * sqrt(kObservationNum - 2) / (1 - sample.correlation ^
      2), 0, qt(1 - 0.05, kObservationNum - 2), type = "original", name = "student"
)

## Pearson's product-moment correlation test. Use time for y as numerical
sample.ctest <-
  cor.test(sample$temperature, c(1:kObservationNum), method = "pearson")
WriteTest(
  sample.ctest$statistic, sample.ctest$p.value, sample.ctest$parameter[[1]], type =
    "original", name = "correlation"
)

## Fitting linear model for researching data. It also compute residuals based on subtracted regression
sample.fit <- lm(sample$temperature ~ c(1:kObservationNum))

linear <- function(x, a, b)
  a * x + b
pr.trend <-
  sapply(
    X = ConvertYearsToNum(src$year[(kObservationNum + 1):nrows]), FUN = linear, a =
      sample.fit$coefficients[[2]], b = sample.fit$coefficients[[1]]
  )
sample.residuals.prediction.trend <-
  data.frame(
    "Год" = src$year[(kObservationNum + 1):nrows],
    "Актуальное" = src$temperature[(kObservationNum + 1):nrows],
    "Прогнозное" = pr.trend,
    "Ошибка" = src$temperature[(kObservationNum + 1):nrows] - pr.trend
  )
pr.mse <-
  MSE(src$temperature[(kObservationNum + 1):nrows] - pr.trend)
colnames(sample.residuals.prediction.trend) <-
  c("", "$X(t)$", "$y(t)$", "$ X(t) - y(t) $")
print(
  xtable(
    sample.residuals.prediction.trend, caption = "Сравнение прогнозных значений (модель $ y(t) $)", label =
      "table:prediction_trend", digits = c(0, 0, 3, 3, 3), align = "rr|ccc"
  ), caption.placement = 'top',
  file = "out/residual/prediction-trend.tex", sanitize.text.function = function(x) {
    x
  }, include.rownames = FALSE
)

## Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.data.ts <-
  DrawTimeSeries(data = sample, filename = "original/time-series.png", datebreaks =
      kDateBreaks)

## Next step is research residuals computed few lines above
sample.residuals <-
  data.frame("year" = sample$year, "temperature" = sample.fit$residuals)
tmp <- sample.residuals
colnames(tmp) <- c("Год", "Температура, ºС")
print(
  xtable(
    tmp, caption = "Временной ряд остатков", label = "table:residuals", digits =
      c(0, 0, 2), align = "r|rc|"
  ),  table.placement = "H", caption.placement = 'top',
  file = "out/residual/data.tex", include.rownames = FALSE
)

sign <- regr.significance(sample$temperature, write = TRUE)
adeq <- regr.adequacy(sample$temperature, write = TRUE)

## Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <-
  DrawTimeSeries(data = sample.residuals, filename = "residual/time-series.png", datebreaks =
      kDateBreaks)

## Descriptive statistics for residuals
sample.residuals.dstats <-
  dstats.describe(sample.residuals$temperature, type = "residual", locale =
      TRUE)
print(
  xtable(
    sample.residuals.dstats, caption = "Описательные статистики остатков", label =
      "table:residuals_dstats"
  ), caption.placement = 'top',
  file = "out/residual/dstats.tex"
)

## Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <-
  DrawHistogram(data = sample.residuals, filename = "residual/histogram.png")

## Tests for normality
sample.shapiro <-
  ntest.ShapiroWilk(data = sample.residuals$temperature, type = "residual", name =
      "shapiro")
sample.pearson <-
  ntest.PearsonChi2(data = sample.residuals$temperature, type = "residual", name =
      "pearson")
sample.ks      <-
  ntest.KolmogorovSmirnov(data = sample.residuals$temperature, type = "residual", name =
      "ks")

## Normal Quantile-Quantile plot for residuals
plot.residuals.qq <-
  DrawQuantileQuantile(data = sample.residuals$temperature, filename = "residual/quantile.png")

## Auto Correlation Function plot
plot.residuals.acf <-
  DrawAutoCorrelationFunction(data = sample$temperature, filename = "residual/acf.png")

## Box-Ljung and adf tests (some kind of stationarity and independence tests) // TODO: need to know exactly in theory what it is
sample.residuals.box <-
  Box.test(sample.residuals$temperature, type = "Ljung-Box")
WriteTest(
  sample.residuals.box$statistic, sample.residuals.box$p.value, sample.residuals.box$parameter[[1]], type =
    "residual", name = "ljung-box"
)

sample.residuals.adf <- adf.test(sample.residuals$temperature)
WriteTest(
  sample.residuals.adf$statistic, sample.residuals.adf$p.value, type = "residual", name =
    "stationarity"
)

source("R/predictor.R")