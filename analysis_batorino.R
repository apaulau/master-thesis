## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(xtable)
library(nortest)
library(aplpack)
library(outliers)
library(tseries)

source("R/misc-fun.R")
source("R/plotting-fun.R")
source("R/print-fun.R")
source("R/dstats.R")

# Reading input data from csv file
data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

# Outputtind data to TeX table
outdata <- data
outdata$Date <- c(1975:2012)
print(xtable(outdata, caption="Исходные данные.", label="table:source"),  table.placement="H",
      file="out/source_data.tex")

Temperature <- data$Temperature
Date <- data$Date

# Plotting received data
to.pdf(figure.ts(Temperature, title="", xlab="Температура", ylab="Частота"),
       "figures/temperature-ts-first-overview.pdf",
       width=6, height=4);


# Plotting histogram for temperature variable
to.pdf(figure.hist(Temperature, title="", xlab="Температура", ylab="Частота"), 
       "figures/temperature-histogram.pdf", width=6, height=4);

# Plotting histogram with fitted normal density curve for temperature variable
to.pdf(figure.hist(Temperature, title="", freq=F, dnorm, xlab="Температура", ylab="Плотность"), 
       "figures/temperature-histogram-dnorm.pdf", width=6, height=4);

# Getting descriptive statistics for temperature
data.dstats <- dstats.describe(Temperature, locale=T)

# Output descriptive statistics to TeX
print(xtable(data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/data_dstats.tex")

# Normal Quantile-Quantile plot
to.pdf(figure.qqnorm(Temperature, title="", xlab="Теоретические квантили", ylab="Выборочные квантили"),
       "figures/temperature-qqnorm.pdf", width=6, height=4)

# Shapiro-Wilk test for normality
shapiro <- shapiro.test(Temperature)
# Output results to TeX
to.file(shapiro, "out/shapiro.tex")

# Pearson chi-square test for normality
pearson <- pearson.test(Temperature)
# Output results to TeX
to.file(pearson, "out/pearson.tex")

# Kolmogorov-Smirnov test for normality
test.nsample <- rnorm(10000, mean=mean(Temperature), sd=sd(Temperature))
ks <- ks.test(x=Temperature, y=test.nsample, exact=NULL)
# Output results to TeX
to.file(ks, "out/ks.tex")

# Bagplot
to.pdf(figure.bagplot(data, title="", xlab="Дата", ylab="Температура"),
       "figures/bagplot.pdf", width=4, height=3)

# Grubbs test for outliers
grubbs <- grubbs.test(Temperature)
# Output results to TeX
to.file(grubbs, "out/grubbs.tex")

# Correlation matrix
cmatrix <- cor(cbind(Temperature, "Date"=1:length(Temperature)), method="pearson")
print(xtable(cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
      file="out/cmatrix.tex")

# Pearson's product-moment correlation test
time <- 1:length(Temperature) # for y as numerical
to.file(cor.test(Temperature, time, method="pearson"),
        "out/ctest.tex")

# Data scatterplot
to.pdf(figure.scatterplot(data, title="", xlab="Дата", ylab="Температура"),
       "figures/scatterplot.pdf", width=6, height=4)

# Time series with regression line
to.pdf(figure.ts2(data, title="", xlab="Дата", ylab="Температура"),
       "figures/temperature-ts-regression.pdf", width=6, height=4)

# Getting time series for Temperature. Hack: 1969=1975
data.ts <- ts(Temperature, start=c(1969, 7), frequency=1)

# Fitting linear model for data time series
fit <- lm(data.ts ~ time)

data.residuals <- fit$residuals

# Plot detrended time series
to.pdf(figure.residuals(Date, data.residuals, title="", xlab="Дата", ylab="Остатки"),
       "figures/temperature-ts-detrended.pdf", width=6, height=4)

resdata <- data.frame("Year"=outdata$Date, "Residual"=residuals.get(data.residuals))

print(xtable(resdata, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residuals_data.tex")

# Getting descriptive statistics for temperature residuals
resdata.dstats <- dstats.describe(resdata$Residual, locale=T)

# Output descriptive statistics for residuals to TeX
print(xtable(resdata.dstats, caption="Описательные статистики для остатков.", label="table:resid_dstats"),
      file="out/residuals_dstats.tex")

# Plotting histogram with fitted normal density curve for residuals
to.pdf(figure.hist(resdata$Residual, title="", freq=F, dnorm, xlab="Температура", ylab="Дата"), 
       "figures/residuals-histogram-dnorm.pdf", width=6, height=4);

# Normal Quantile-Quantile plot for residuals
to.pdf(figure.qqnorm(resdata$Residual, title="", xlab="Теоретические квантили", ylab="Выборочные квантили"),
       "figures/residuals-qqnorm.pdf", width=6, height=4)

Residual <- resdata$Residual

# Shapiro-Wilk test for normality
resdata.shapiro <- shapiro.test(Residual)
# Output results to TeX
to.file(resdata.shapiro, "out/residuals_shapiro.tex")

# Pearson chi-square test for normality
resdata.pearson <- pearson.test(Residual)
# Output results to TeX
to.file(resdata.pearson, "out/residuals_pearson.tex")

# Kolmogorov-Smirnov test for normality
test.nsample <- rnorm(10000, mean=mean(Residual), sd=sd(Residual))
resdata.ks <- ks.test(x=Residual, y=test.nsample, exact=NULL)
# Output results to TeX
to.file(ks, "out/residuals_ks.tex")

# ACF
to.pdf(figure.acf(Residual), "figures/residuals-acf.pdf", width=6, height=4)
to.file(Box.test(Residual, type="Ljung-Box"), "out/residuals_ljung.tex")
to.file(adf.test(Residual), "out/residuals_stationarity.tex")
