## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(xtable)
source("R/plotting-fun.R")
source("R/print-fun.R")
source("R/dstats.R")
source("R/tests-fun.R")

# Reading input data from csv file
data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

print(data)

# Plotting received data
to.pdf(figure.ts(data$Temperature, title="", ylab="Temperature"),
       "figures/temperature-ts-first-overview.pdf",
       width=6, height=4);


# Plotting histogram for temperature variable
to.pdf(figure.hist(data$Temperature, "Histogram of Temperature"), 
       "figures/temperature-histogram.pdf", width=6, height=4);

# Plotting histogram with fitted normal density curve for temperature variable
to.pdf(figure.hist(data$Temperature, "Histogram with fitted normal density curve", freq=F, dnorm), 
       "figures/temperature-histogram-dnorm.pdf", width=6, height=4);

# Getting descriptive statistics for temperature
dstats <- dstats.describe(data$Temperature, T)

sink(file="out/dstats.tex")
xtable(dstats, caption="Описательные статистики для наблюдаемых температур", label="table:dstats")
sink()

# Normal Quantile-Quantile plot
to.pdf(figure.qqnorm(data$Temperature),
       "figures/temperature-qqnorm.pdf")

shapiro.test(data$Temperature)