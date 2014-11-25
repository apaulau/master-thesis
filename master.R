## Let's start from beginning.
## This file will be the master file of all diploma project's files (slaves). 
## Content will be the same as for previous works (batorino analysis).
## Some thoughts for this investigation see in TODO.Rmd.

# First of all we need to clean up the workspace
rm(list=ls(all=TRUE))

# Dependencies
library(ggplot2)  # eye-candy graphs
library(xtable)   # convert data to latex tables
library(nortest)  # tests for normality
library(outliers) # tests for outliers

# 8< --- check do I need this packages
library(tseries)  # introduces time series
library(aplpack)  # don't remember what is it
# --- >8

# Import local modules
source("R/misc-fun.R")     # some useful functions // TODO: check usages and usefulness
source("R/plotting-fun.R") # useful functions for more comfortable plotting // TODO: check it too
source("R/print-fun.R")    # functions for print some data to files
source("R/dstats.R")       # descriptive statistics module

# Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choose multiple data sources
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=38, colClasses = c("numeric", "numeric"), stringsAsFactors=FALSE)

# Global use constants // TODO: try to use more narrow scope
kStartYear  <- min(src.data$year)
kEndYear    <- max(src.data$year)
kDateBreaks <- seq(kStartYear - 5, kEndYear + 5, by=2) # date points for graphs // TODO: investigate how to skip this usage

# For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src.data[, 1]) - 3

# Form the data for research
research.data <- src.data[0 : kObservationNum, ]

# Output source data to latex table. So we can reference to it in docs.
print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", file="out/source_data.tex")

## Computation and analysis block

# Fitting linear model for researching data. It also compute residuals based on subtracted regression
data.fit <- lm(Temperature ~ time)

# Next step is research residuals computed few lines above
research.residuals <- data.frame("year"=research.data$year, "temperature"=research.data$residuals)

# Auto Correlation Function computation and definition
research.residuals.ci     <- .95 # confidence interval
research.residuals.acf    <- acf(resdata$residuals, plot=FALSE, lag.max=30)
research.residuals.acfdf  <- data.frame(acf=acf$acf, lag=acf$lag) # data frame for computed ACF
research.residuals.clim   <- qnorm((1 + ci) / 2) / sqrt(acf$n.used) # limit // TODO: find out where it from / or remember what is it for

## Plots computation block

# Source data as basic time series plot: points connected with line
plot.src.base <- ggplot(src.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")

# Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.src.hist <- ggplot(research.data, aes(x=temperature), geom = 'blank') +   
  geom_histogram(aes(y = ..density..), colour="darkgrey", fill="white", 
                 binwidth = diff(range(src.data$temperature)) / nclass.Sturges(src.data$temperature), alpha=.6) +
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")

# The same as previous histogram but with fitted normal distribution density curve
plot.src.hist.norm <- plot.hist +
  stat_function(fun = dnorm, colour='red', geom = 'line', 
                arg = list(mean=mean(src.data$temperature), sd=sd(src.data$temperature)))

## 8< --- move this to the separate module
# Q-Q plot
ggqqp <- function (vec) {
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    xlab("Теоретические квантили") + ylab("Выборочные квантили")
}
## >8 ---

# Normal Quantile-Quantile plot
plot.src.qq <- ggqqp(data$temperature)


# Bagplot. // TODO: Investigate how to replace this with new style: ggplot
to.pdf(figure.bagplot(data.frame("Date"=research.data$year, "Temperature"=research.data$temperature), title="", xlab="Год", ylab="Температура"),
       "figures/05_bagplot.pdf", width=4, height=3)

# Scatter plot with regression line based on some magic; investigate again what is this magic is // TODO: look up can I introduce new variable with y-breaks
plot.src.scatter <- ggplot(data, aes(x=year, y=temperature)) + 
  geom_point() + geom_abline(intercept=-194.632277, slope=0.107706, color="blue") +
  scale_x_continuous(limits=c(min(research.data$year) - 5, max(research.data$year) + 5), breaks=kDateBreaks) + 
  scale_y_continuous(breaks=seq(10, 30, 1), limits=c(14, 26)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")

# Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.src.ts <- ggplot(research.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + stat_smooth(method=lm, se=FALSE) + 
  scale_x_continuous(breaks=kDateBreaks) + scale_y_continuous(breaks=seq(16, 28, 1)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")

# Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <- ggplot(research.residuals, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")

# Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <- ggplot(research.residuals, aes(x=temperature), geom='blank') +   
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=1.2, alpha=.6) +
  stat_function(fun=dnorm, colour='red', arg=list(mean=mean(research.residuals$temperature), sd=sd(research.residuals$temperature))) +    
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")

# Normal Quantile-Quantile plot for residuals
plot.residuals.qq <- qqp(research.residuals$temperature)

# Auto Correlation Function plot // TODO: check the style
plot.residuals.acf <- ggplot(data = research.residuals.acfdf, mapping=aes(x=lag, y=acf)) +
  geom_hline(colour = "grey50") + geom_hline(yintercept=c(-clim, clim), linetype="dashed", col="blue") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  labs(color="") + xlab("Лаг") + ylab("Автокорреляция")