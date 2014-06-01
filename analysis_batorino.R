## Analysis for lake Batorino, sample is observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is year in YYYY format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(xtable)
library(nortest)
library(aplpack)
library(outliers)
library(tseries)
library(ggplot2)

source("R/misc-fun.R")
source("R/plotting-fun.R")
source("R/print-fun.R")
source("R/dstats.R")

# Reading input data from csv file, observations number is 38.
src <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("numeric", "numeric"), stringsAsFactors=F)

START_YEAR <- min(src$year)
END_YEAR <- max(src$year)

# Outputtind source data to TeX table
print(xtable(src, caption="Исходные данные.", label="table:source"),  table.placement="H",
      file="out/source_data.tex")

datebreaks <- seq(START_YEAR-5, END_YEAR+5, by=2)

# Plotting received data
plot.src <- ggplot(src, aes(x=year, y=temperature)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=datebreaks) + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
ggsave(plot=plot.src, file="figures/01_src.png", width=7, height=4)

# For the reason of future predictions comparison, let initial observations number will be 35
OBS_NUM <- 35

# Forming data for research
Temperature <- src$temperature[1:OBS_NUM]
Year <- src$year[1:OBS_NUM]

data <- data.frame("year"=Year, "temperature"=Temperature)

# Plotting histogram for temperature variable
plot.hist <- ggplot(data, aes(x=temperature), geom = 'blank') +   
  geom_histogram(aes(y = ..density..), colour="darkgrey", fill="white", 
                 binwidth = diff(range(src$temperature)) / nclass.Sturges(src$temperature), alpha = 0.6) +
  labs(color="") +
  ylab("Плотность") + xlab("Температура, ºС")
ggsave(plot=plot.hist, file="figures/02_hist.png", width=7, height=4)

# Plotting histogram with fitted normal density curve for temperature variable
plot.hist.norm <- ggplot(data, aes(x=temperature), geom = 'blank') +   
  geom_histogram(aes(y = ..density..),colour="darkgrey", fill="white", 
                 binwidth=diff(range(src$temperature)) / nclass.Sturges(src$temperature), alpha = 0.6) +
  stat_function(fun = dnorm, colour='red', 
                arg = list(mean = mean(src$temperature), sd=sd(src$temperature)),
                geom = 'line') +    
  labs(color="") +
  ylab("Плотность") + xlab("Температура, ºС")
ggsave(plot=plot.hist.norm, file="figures/03_hist-dnorm.png", width=7, height=4)

# Getting descriptive statistics for temperature
data.dstats <- dstats.describe(Temperature, locale=T)

# Output descriptive statistics to TeX
print(xtable(data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/data_dstats.tex")

# Q-Q plot
qqp <- function (vec) {
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    xlab("Теоретические квантили") + ylab("Выборочные квантили")
}

# Normal Quantile-Quantile plot
plot.qq <- qqp(data$temperature)
ggsave(plot=plot.qq, file="figures/04_qq.png", width=7, height=4)

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
to.pdf(figure.bagplot(data.frame("Date"=data$year, "Temperature"=data$temperature), title="", xlab="Год", ylab="Температура"),
       "figures/05_bagplot.pdf", width=4, height=3)

# Grubbs test for outliers
grubbs <- grubbs.test(Temperature)
# Output results to TeX
to.file(grubbs, "out/grubbs.tex")

# Correlation matrix
cmatrix <- cor(cbind(Temperature, "Date"=1:length(Temperature)), method="pearson")
print(xtable(cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
      file="out/cmatrix.tex")

# Pearson's product-moment correlation test
time <- 1:OBS_NUM # for y as numerical
to.file(cor.test(Temperature, time, method="pearson"),
        "out/ctest.tex")

# Data scatterplot
plot.scatter <- ggplot(data, aes(x=year, y=temperature)) + 
  geom_point() + scale_x_continuous(limits=c(min(data$year)-5, max(data$year)+5), breaks=datebreaks) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(10,30,1), limits=c(14,26)) +
  xlab("Год наблюдения") + ylab("Температура, ºС") + geom_abline(intercept=-194.632277, slope=0.107706, color="blue")
ggsave(plot=plot.scatter, file="figures/06_scatterplot.png", width=7, height=4)

# Time series with regression line
plot.ts <- ggplot(data, aes(x=year, y=temperature)) + geom_point() + geom_line() + stat_smooth(method=lm, se=F) + 
  scale_x_continuous(breaks=datebreaks) + xlab("Год наблюдения") + ylab("Температура, ºС") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(16,28,1))
ggsave(plot=plot.ts, file="figures/07_ts.png", width=7, height=4)

# Fitting linear model for data time series
data.fit <- lm(Temperature ~ time)
data$residuals <- data.fit$residuals

# Plot detrended time series
plot.residuals <- ggplot(data, aes(x=year, y=residuals)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=datebreaks) + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
ggsave(plot=plot.residuals, file="figures/08_residuals.png", width=7, height=4)

resdata <- data.frame("year"=data$year, "residual"=data$residuals)

print(xtable(resdata, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residuals_data.tex")

# Getting descriptive statistics for temperature residuals
resdata.dstats <- dstats.describe(resdata$residual, locale=T)

# Output descriptive statistics for residuals to TeX
print(xtable(resdata.dstats, caption="Описательные статистики для остатков.", label="table:resid_dstats"),
      file="out/residuals_dstats.tex")

# Plotting histogram with fitted normal density curve for residuals
plot.res.hist <- ggplot(resdata, aes(x=residual), geom = 'blank') +   
  geom_histogram(aes(y = ..density..), colour="darkgrey", fill="white", 
                 binwidth=1.2, alpha = 0.6) +
  stat_function(fun = dnorm, colour='red',
                arg = list(mean = mean(resdata$residual), sd=sd(resdata$residual))) +    
  labs(color="") +
  ylab("Плотность") + xlab("Температура, ºС")
ggsave(plot=plot.res.hist, file="figures/09_res-hist-dnorm.png", width=7, height=4)

# Normal Quantile-Quantile plot for residuals
plot.qq <- qqp(resdata$residual)
ggsave(plot=plot.qq, file="figures/10_res-qq.png", width=7, height=4)

Residual <- resdata$residual

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
ci <- .95
acf <- acf(resdata$residual, plot=F, lag.max=30)
acfdf <- data.frame(
  acf = acf$acf,
  lag = acf$lag
)
clim <- qnorm((1 + ci)/2) / sqrt(acf$n.used)

plot.res.acf <- ggplot(data = acfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(colour = "grey50") + 
  geom_hline(yintercept=c(-clim, clim), linetype="dashed", col="blue") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  labs(color="") +
  ylab("Автокорреляция") + xlab("Лаг")
ggsave(plot=plot.res.acf, file="figures/11_acf.png", width=7, height=4)

to.file(Box.test(Residual, type="Ljung-Box"), "out/residuals_ljung.tex")
to.file(adf.test(Residual), "out/residuals_stationarity.tex")