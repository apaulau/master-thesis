## Cleaning up the workspace
rm(list=ls(all=TRUE))

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

## Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

## Global use constants
kDateBreaks <- seq(min(src.data$year) - 5, max(src.data$year) + 5, by=2) # date points for graphs

## For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src.data[, 1]) - 3
WriteCharacteristic(expression=kObservationNum, type="original", name="n")

## Source data as basic time series plot: points connected with line
plot.source <- DrawDataRepresentation(data=src.data, filename="source.png", datebreaks=kDateBreaks)

print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", 
      file="out/original/data.tex")

## Form the data for research
research.data <- src.data[0:kObservationNum, ]

# Getting descriptive statistics for temperature in russian locale
research.data.dstats <- dstats.describe(research.data$temperature, type="original", locale=TRUE)
print(xtable(research.data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/original/dstats.tex")

# Compute Sturges rule for output
WriteCharacteristic(expression=nclass.Sturges(research.data$temperature), type="original", name="sturges")

## Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.data.hist <- DrawHistogram(data=research.data, filename="original/histogram.png")

## Tests for normality
research.data.shapiro <- ntest.ShapiroWilk(data=research.data$temperature, type="original", name="shapiro") 
research.data.pearson <- ntest.PearsonChi2(data=research.data$temperature, type="original", name="pearson")
research.data.ks      <- ntest.KolmogorovSmirnov(data=research.data$temperature, type="original", name="ks")

## Normal Quantile-Quantile plot // TODO: check when it appears in text
plot.data.qq <- DrawQuantileQuantile(data=research.data$temperature, filename="original/quantile.png")

## Scatter plot with regression line
plot.data.scatter <- DrawScatterPlot(research.data, filename="original/scatterplot.png", kDateBreaks);

## Grubbs test for outliers
research.data.grubbs <- grubbs.test(research.data$temperature)
WriteTest(research.data.grubbs$statistic, research.data.grubbs$p.value, type="original", name="grubbs")

## Compute correlation for output
research.data.correlation <- cor(x=research.data$year, y=research.data$temperature)
WriteCharacteristic(research.data.correlation, type="original", name="correlation")

## Pearson's product-moment correlation test. Use time for y as numerical
research.data.ctest <- cor.test(research.data$temperature, c(1:kObservationNum), method="pearson")
WriteTest(research.data.ctest$statistic, research.data.ctest$p.value, research.data.ctest$parameter[[1]], type="original", name="correlation")

## Fitting linear model for researching data. It also compute residuals based on subtracted regression
research.data.fit <- lm(research.data$temperature ~ c(1:kObservationNum))

linear <- function(x, a, b) a * x + b
research.residuals.prediction.trend <- data.frame("Год"=src.data$year[(kObservationNum + 1):src.nrows],
                                                  "Актуальное"=src.data$temperature[(kObservationNum + 1):src.nrows],
                                                  "Прогнозное"=sapply(X=ConvertYearsToNum(src.data$year[(kObservationNum + 1):src.nrows]), FUN=linear, a=research.data.fit$coefficients[[2]], b=research.data.fit$coefficients[[1]]))
print(xtable(research.residuals.prediction.trend, caption="Сравнение прогнозных значений", label="table:prediction_trend", digits=c(0, 0, 2, 2)),
  file="out/residual/prediction-trend.tex")

## Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.data.ts <- DrawTimeSeries(data=research.data, filename="original/time-series.png", datebreaks=kDateBreaks)

## Next step is research residuals computed few lines above
research.residuals <- data.frame("year"=research.data$year, "temperature"=research.data.fit$residuals)
print(xtable(research.residuals, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residual/data.tex")

## Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <- DrawTimeSeries(data=research.residuals, filename="residual/time-series.png", datebreaks=kDateBreaks)

## Descriptive statistics for residuals
research.residuals.dstats <- dstats.describe(research.residuals$temperature, type="residual", locale=TRUE)
print(xtable(research.residuals.dstats, caption="Описательные статистики остатков", label="table:residuals_dstats"),
      file="out/residual/dstats.tex")

## Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <- DrawHistogram(data=research.residuals, filename="residual/histogram.png")

## Tests for normality
research.data.shapiro <- ntest.ShapiroWilk(data=research.residuals$temperature, type="residual", name="shapiro") 
research.data.pearson <- ntest.PearsonChi2(data=research.residuals$temperature, type="residual", name="pearson")
research.data.ks      <- ntest.KolmogorovSmirnov(data=research.residuals$temperature, type="residual", name="ks")

## Normal Quantile-Quantile plot for residuals
plot.residuals.qq <- DrawQuantileQuantile(data=research.residuals$temperature, filename="residual/quantile.png")

## Auto Correlation Function plot
plot.residuals.acf <- DrawAutoCorrelationFunction(data=research.data$temperature, filename="residual/acf.png")

## Box-Ljung and adf tests (some kind of stationarity and independence tests) // TODO: need to know exactly in theory what it is
research.residuals.box <- Box.test(research.residuals$temperature, type="Ljung-Box")
WriteTest(research.residuals.box$statistic, research.residuals.box$p.value, research.residuals.box$parameter[[1]], type="residual", name="ljung-box")

research.residuals.adf <- adf.test(research.residuals$temperature)
WriteTest(research.residuals.adf$statistic, research.residuals.adf$p.value, type="residual", name="stationarity")

source("R/predictor.R")