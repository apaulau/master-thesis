# This module is for tests for normality

library(nortest)  # tests for normality // WANT TO STEAL

worker <- function (test, data, type, name, ...) {
  result <- test(data, ...)
  if (nchar(name)) {
    WriteTest(result$statistic, result$p.value, type=type, name=name)
  }
  
  result
}

# Shapiro-Wilk test for normality
ntest.ShapiroWilk <- function (data, type, name="", ...) {
  worker(shapiro.test, data, type=type, name=name, ...)  
}

ntest.PearsonChi2 <- function (data, type, name="", ...) {
  worker(pearson.test, data, type=type, name=name, ...)  
}

ntest.KolmogorovSmirnov <- function (data, type, name="", ...) {
  nsample <- rnorm(10000, mean=mean(data), sd=sd(data)) # sample for test against source 
  worker(ks.test, data, type=type, name=name, y=nsample, ...)  
}
