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
kObservationNum <- length(src.data) - 3

# Form the data for research
research.data <- src.data[0 : kObservationNum, ]

# Output source data to latex table. So we can reference to it in docs.
print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", file="out/source_data.tex")


## Plots computation block

# Source data as basic time series plot: points connected with line
plot.src <- ggplot(src, aes(x=year, y=temperature)) + geom_point() + 
  geom_line() + scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")


# Structure in mind: first of all computation, and after all printing
# But. In this way if you need to recompute some part of all work then you should seek at least two places in code and execute each.
# So this point is for 