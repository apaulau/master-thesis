# This module is for tests for normality

library(nortest)  # tests for normality // WANT TO STEAL

worker <- function (test, data, filename, ...) {
  result <- test(data, ...)
  if (nchar(filename)) {
    to.file(result, filename)
  }
  
  result
}

# Shapiro-Wilk test for normality
ntest.ShapiroWilk <- function (data, filename="", ...) {
  worker(shapiro.test, data, filename, ...)  
}

ntest.PearsonChi2 <- function (data, filename="", ...) {
  worker(pearson.test, data, filename, ...)  
}

ntest.KolmogorovSmirnov <- function (data, filename="", ...) {
  nsample <- rnorm(10000, mean=mean(data), sd=sd(data)) # sample for test against source 
  worker(ks.test, data, filename, y=nsample, ...)  
}
