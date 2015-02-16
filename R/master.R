# Let's start from beginning.
# This file will be the master file of all diploma project's files (slaves). 
# Content will be the same as for previous works (batorino analysis).
# Some thoughts for this investigation see in TODO.Rmd.
# Ideas for organizing further research see in ideas.Rmd

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
source("R/lib/print.R")      # functions for print some data to files
source("R/lib/dstats.R")     # descriptive statistics module
source("R/lib/misc.R")       # some useful global-use functions
source("R/lib/draw.R")       # helpers for drawing
source("R/lib/ntest.R")      # tests for normality

## Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

## Global use constants
kDateBreaks <- seq(min(src.data$year) - 5, max(src.data$year) + 5, by=2) # date points for graphs

## For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src.data[, 1]) - 3

## Source data as basic time series plot: points connected with line
plot.source <- DrawDataRepresentation(data=src.data, filename="source.png", datebreaks=kDateBreaks)

print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", 
      file="out/original/data.tex")

## Form the data for research
research.data <- src.data[0:kObservationNum, ]

# Getting descriptive statistics for temperature in russian locale
research.data.dstats <- dstats.describe(research.data$temperature, locale=TRUE)
print(xtable(research.data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/original/dstats.tex")

## Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.data.hist <- DrawHistogram(data=research.data, filename="original/histogram.png", datebreaks=kDateBreaks)

## Tests for normality
research.data.shapiro <- ntest.ShapiroWilk(data=research.data$temperature, filename="out/original/shapiro-test.tex") 
research.data.pearson <- ntest.PearsonChi2(data=research.data$temperature, filename="out/original/pearson-test.tex")
research.data.ks      <- ntest.KolmogorovSmirnov(data=research.data$temperature, filename="out/original/ks-test.tex")

## Normal Quantile-Quantile plot // TODO: check when it appears in text
plot.data.qq <- DrawQuantileQunatile(data=research.data$temperature, filename="original/quantile.png")

## Scatter plot with regression line
plot.data.scatter <- DrawScatterPlot(research.data, filename="original/scatterplot.png", kDateBreaks);

## Grubbs test for outliers
research.data.grubbs <- grubbs.test(research.data$temperature)
to.file(research.data.grubbs, "out/original/grubbs-test.tex")

## Correlation matrix
research.data.cmatrix <- cor(cbind("Temperature"=research.data$temperature, "Date"=1:kObservationNum), method="pearson")
print(xtable(research.data.cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
      file="out/original/corr-matrix.tex")
                             
## Pearson's product-moment correlation test. Use time for y as numerical
research.data.ctest <- cor.test(research.data$temperature, c(1:kObservationNum), method="pearson")
to.file(research.data.ctest, "out/original/corr-test.tex")

## Fitting linear model for researching data. It also compute residuals based on subtracted regression
research.data.fit <- lm(research.data$temperature ~ c(1:kObservationNum))

## Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.data.ts <- DrawTimeSeries(data=research.data, filename="original/time-series.png", datebreaks=kDateBreaks)

## Next step is research residuals computed few lines above
research.residuals <- data.frame("year"=research.data$year, "temperature"=research.data.fit$residuals)
print(xtable(research.residuals, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residual/data.tex")

## Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <- DrawTimeSeries(data=research.residuals, filename="residual/time-series.png", datebreaks=kDateBreaks)

## Descriptive statistics for residuals
research.residuals.dstats <- dstats.describe(research.residuals$temperature, locale=TRUE)
print(xtable(research.residuals.dstats, caption="Описательные статистики для остатков.", label="table:residuals_dstats"),
      file="out/residuals_dstats.tex")

## Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <- DrawHistogram(data=research.residuals, filename="residual/histogram.png", datebreaks=kDateBreaks)

## Tests for normality
research.data.shapiro <- ntest.ShapiroWilk(data=research.residuals$temperature, filename="out/residual/shapiro-test.tex") 
research.data.pearson <- ntest.PearsonChi2(data=research.residuals$temperature, filename="out/residual/pearson-test.tex")
research.data.ks      <- ntest.KolmogorovSmirnov(data=research.residuals$temperature, filename="out/residual/ks-test.tex")

## Normal Quantile-Quantile plot for residuals
plot.residuals.qq <- DrawQuantileQunatile(data=research.residuals$temperature, filename="residual/qunatile.png")

## Auto Correlation Function plot // TODO: check the style
plot.residuals.acf <- DrawAutoCorrelationFunction(data=research.data$temperature, filename="residual/acf.png")

## Box-Ljung and adf tests (some kind of stationarity and independence tests) // TODO: need to know exactly in theory what it is
research.residuals.box <- Box.test(research.residuals$temperature, type="Ljung-Box")
to.file(research.residuals.box, "out/residual/ljung-test.tex")
research.residuals.adf <- adf.test(research.residuals$temperature)
to.file(research.residuals.adf, "out/residual/stationarity-test.tex")


source("R/variogram.R")