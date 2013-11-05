## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(psych)

source("R/plotting-fun.R")
source("R/print-fun.R")

# Reading input data from csv file
data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

print(data)

# Plotting received data
to.pdf(figure.ts(data), "figures/temperature-ts-first-overview.pdf",
       width=6, height=4)

# Plotting histogram for temperature variable
to.pdf(figure.hist(data), "figures/temperature-histogram-1.pdf",
       width=6, height=4);

dstats <- describe(data$Temperature)

