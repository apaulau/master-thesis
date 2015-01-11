# This module is for tests for normality

library(nortest)  # tests for normality // WANT TO STEAL

# Shapiro-Wilk test for normality
ntest.shapiro <- function (data, ...) {
  shapiro.test(data)
}