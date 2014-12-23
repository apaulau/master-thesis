# Let's start from beginning.
# This file will be the master file of all diploma project's files (slaves). 
# Content will be the same as for previous works (batorino analysis).
# Some thoughts for this investigation see in TODO.Rmd.
# Ideas for organizing further research see in ideas.Rmd

## First of all we need to clean up the workspace
rm(list=ls(all=TRUE))

## Dependencies
library(ggplot2)  # eye-candy graphs
library(xtable)   # convert data to latex tables
library(outliers) # tests for outliers
library(tseries)  # adf test used
library(aplpack)  # there is bagplot functionality // WANT TO REMOVE but can't without lost bagplot graph
library(nortest)  # tests for normality

## Import local modules
source("R/lib/plotting-fun.R") # useful functions for more comfortable plotting
source("R/lib/print-fun.R")    # functions for print some data to files
source("R/lib/dstats.R")       # descriptive statistics module

#[ Initialize block

## Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=38, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", 
      file="out/source.tex")

## Global use constants
kDateBreaks <- seq(min(src.data$year) - 5, max(src.data$year) + 5, by=2) # date points for graphs

## For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src.data[, 1]) - 3

## Form the data for research
research.data <- src.data[0:kObservationNum, ]

#] End of initialize block
#[ Computation and analysis block

## Fitting linear model for researching data. It also compute residuals based on subtracted regression
research.data.fit <- lm(research.data$temperature ~ c(1:kObservationNum))

# Getting descriptive statistics for temperature in russian locale
research.data.dstats <- dstats.describe(research.data$temperature, locale=TRUE)
print(xtable(research.data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/data_dstats.tex")

##[ Tests for normality

### Shapiro-Wilk
research.data.shapiro <- shapiro.test(research.data$temperature)
to.file(research.data.shapiro, "out/data_shapiro.tex")

### Pearson chi-square
research.data.pearson <- pearson.test(research.data$temperature)
to.file(research.data.pearson, "out/data_pearson.tex")

### Kolmogorov-Smirnov
test.nsample <- rnorm(10000, mean=mean(research.data$temperature), sd=sd(research.data$temperature)) # sample for test against source 
research.data.ks <- ks.test(x=research.data$temperature, y=test.nsample, exact=NULL)
to.file(research.data.ks, "out/data_ks.tex")

##] End tests for normality

## Grubbs test for outliers
research.data.grubbs <- grubbs.test(research.data$temperature)
to.file(research.data.grubbs, "out/data_grubbs.tex")

## Correlation matrix
research.data.cmatrix <- cor(cbind("Temperature"=research.data$temperature, "Date"=1:kObservationNum), method="pearson")
print(xtable(research.data.cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
      file="out/data_cmatrix.tex")
                             
## Pearson's product-moment correlation test. Use time for y as numerical
research.data.ctest <- cor.test(research.data$temperature, c(1:kObservationNum), method="pearson")
to.file(research.data.ctest, "out/ctest.tex")

## Next step is research residuals computed few lines above
research.residuals <- data.frame("year"=research.data$year, "temperature"=research.data.fit$residuals)
print(xtable(research.residuals, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residuals.tex")

## Descriptive statistics for residuals
research.residuals.dstats <- dstats.describe(research.residuals$temperature, locale=TRUE)
print(xtable(research.residuals.dstats, caption="Описательные статистики для остатков.", label="table:residuals_dstats"),
      file="out/residuals_dstats.tex")

##[ Tests for normality // combine with previous normality tests

### Shapiro-Wilk
research.residuals.shapiro <- shapiro.test(research.residuals$temperature)
to.file(research.residuals.shapiro, "out/residuals_shapiro.tex")

### Pearson chi-square
research.residuals.pearson <- pearson.test(research.residuals$temperature)
to.file(research.residuals.pearson, "out/residuals_pearson.tex")

### Kolmogorov-Smirnov
test.nsample <- rnorm(10000, mean=mean(research.residuals$temperature), sd=sd(research.residuals$temperature)) # sample for test against source 
research.residuals.ks <- ks.test(x=research.residuals$temperature, y=test.nsample, exact=NULL)
to.file(research.residuals.ks, "out/residuals_ks.tex")

##] End tests for normality

## Box-Ljung and adf tests (some kind of stationarity and independence tests) // TODO: need to know exactly in theory what it is
research.residuals.box <- Box.test(research.residuals$temperature, type="Ljung-Box")
to.file(research.residuals.box, "out/residuals_ljung.tex")
research.residuals.adf <- adf.test(research.residuals$temperature)
to.file(research.residuals.adf, "out/residuals_stationarity.tex")

#] End computation block
#[ Plots computation block

## Source data as basic time series plot: points connected with line
plot.source <- ggplot(src.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.source, filename="01_src.png")

## Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.data.hist <- ggplot(research.data, aes(x=temperature), geom='blank') +   
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", 
                 binwidth=diff(range(src.data$temperature)) / nclass.Sturges(src.data$temperature), alpha=.6) +
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
plot.save(plot.data.hist, filename="02_hist.png")

## The same as previous histogram but with fitted normal distribution density curve
plot.data.hist.norm <- plot.data.hist +
  stat_function(fun=dnorm, colour='red', geom='line', 
                arg=list(mean=mean(src.data$temperature), sd=sd(src.data$temperature)))
plot.save(plot.data.hist.norm, filename="03_hist-dnorm.png")

## Normal Quantile-Quantile plot
plot.data.qq <- ggqqp(research.data$temperature)
plot.save(plot.data.qq, filename="04_qq.png")

## Bagplot. // TODO: Investigate how to replace this with new style: ggplot
#plot.data.bag <- figure.bagplot(data.frame("Date"=research.data$year, "Temperature"=research.data$temperature), title="", xlab="Год", ylab="Температура")
#to.pdf(plot.data.bag, "figures/05_bagplot.pdf", width=4, height=3)

## Scatter plot with regression line based on some magic; investigate again what is this magic is // TODO: look up can I introduce new variable with y-breaks
plot.data.scatter <- ggplot(research.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_abline(intercept=-194.632277, slope=0.107706, color="blue") +
  scale_x_continuous(limits=c(min(research.data$year) - 5, max(research.data$year) + 5), breaks=kDateBreaks) + 
  scale_y_continuous(breaks=seq(10, 30, 1), limits=c(14, 26)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.data.scatter, filename="06_scatterplot.png")

## Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.data.ts <- ggplot(research.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + stat_smooth(method=lm, se=FALSE) + 
  scale_x_continuous(breaks=kDateBreaks) + scale_y_continuous(breaks=seq(16, 28, 1)) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.data.ts, filename="07_ts.png")

## Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <- ggplot(research.residuals, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.residuals.ts, filename="08_residuals.png")

## Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <- ggplot(research.residuals, aes(x=temperature), geom='blank') +   
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=1.2, alpha=.6) +
  stat_function(fun=dnorm, colour='red', arg=list(mean=mean(research.residuals$temperature), sd=sd(research.residuals$temperature))) +    
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
plot.save(plot.residuals.hist, filename="09_res-hist-dnorm.png")

## Normal Quantile-Quantile plot for residuals
plot.residuals.qq <- ggqqp(research.residuals$temperature)
plot.save(plot.residuals.qq, filename="10_res-qq.png")

## Auto Correlation Function plot // TODO: check the style

plot.residuals.acf <- ggacf(research.data$temperature)
plot.save(plot.residuals.acf, filename="11_acf.png")

#] End plots block