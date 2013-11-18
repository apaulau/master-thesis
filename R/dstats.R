# Descriptive statistics

dstats.mean <- function(data, ...) {
  mean(data, ...)
}

dstats.median <- function(data, ...) {
  median(data, ...)
}

dstats.range <- function(data) {
  max(data) - min(data) 
}

dstats.variance <- function(data, ...) {
  var(data, ...)
}

dstats.std.dev <- function(data) {
  sqrt(var(data))
}

dstats.coef.var <- function(data) {
  (var(data) / mean(data)) * 100%
}

dstats.std.error <- function(data) {
  sqrt(var(data) / n)
}

dstats.skew <- function(data) {
  n <- length(data)
  mean <- mean(data)
  (n * sum(sapply(data, FUN=function(x){(x - mean)^3}))) /
    ((n - 1)*(n - 2)*dstats.std.dev(data)^3)  
}

dstats.std.error.skew <- function(data) {
  n <- length(data)
  sqrt((6*n*(n - 1)) / ((n - 2)*(n + 1)*(n + 3)))
}

dstats.test.skew <- function(data) {
  dstats.skew(data) / dstats.std.error.skew
}

dstats.kurtosis <- function(data) {
  n <- length(data)
  mean <- mean(data)
  (n*(n + 1)*sum(sapply(data, FUN=function(x){(x - mean)^4})) - 3*(sum(sapply(data, FUN=function(x){(x - mean)^2})))^2*(n - 1)) / 
    ((n - 1)*(n - 2)*(n - 3)*dstats.variance(data)^2)
}

dstats.std.error.kurtosis <- function(data) {
  n <- length(data)
  2 * dstats.std.error.skew * sqrt((n^2 - 1) / ((n - 3)*(n + 5)))
}

dstats.test.kurtosis <- function(data) {
  dstats.kurtosis(data) / dstats.std.error.kurtosis
}