# Descriptive statistics

# Function for getting all descriptive statistics
dstats.describe <- function(data, locale=F) {
  stats <- c(dstats.mean(data), dstats.median(data), dstats.quartile.lower(data),
             dstats.quartile.upper(data), dstats.min(data), dstats.max(data),
             dstats.range(data), dstats.quartile.range(data), dstats.variance(data),
             dstats.std.dev(data), dstats.coef.var(data), dstats.std.error(data),
             dstats.skew(data), dstats.std.error.skew(data), dstats.kurtosis(data),
             dstats.std.error.kurtosis(data))
  if (locale) {
    descr.row <- c("Среднее", "Медиана", "Нижний квартиль", "Верхний квартиль", 
                   "Минимум", "Максимум", "Размах", "Квартильный размах",
                   "Дисперсия", "Стандартное отклонение", "Коэффициент вариации",
                   "Стандартная ошибка", "Асимметрия", "Ошибка асимметрии",
                   "Эксцесс", "Ошибка эксцесса")
    descr.col <- c("Значение")
  } else {
    descr.row <- c("Mean", "Median", "Lower Quartile", "Upper Quartile", "Range",
                   "Minimum", "Maximum", "Quartile Range", "Variance", "Standard Deviation", 
                   "Coefficient of Variance", "Standard Error", "Skewness", 
                   "Std. Error Skewness", "Kurtosis", "Std. Error Kurtosis")
    descr.col <- c("Value")
  }
  df <- data.frame(stats, row.names=descr.row) 
  colnames(df) <- descr.col
  
  df
}

dstats.mean <- function(data, ...) {
  mean(data, ...)
}

dstats.median <- function(data, ...) {
  median(data, ...)
}

dstats.quartile.lower <- function(data, ...) {
  quantile(data, ...)[[2]]
}

dstats.quartile.upper <- function(data, ...) {
  quantile(data, ...)[[4]]
}

dstats.quartile.range <- function(data) {
  dstats.quartile.upper(data) - dstats.quartile.lower(data)
}

dstats.min <- function(data, ...) {
  min(data, ...)
}

dstats.max <- function(data, ...) {
  max(data, ...)
}

dstats.range <- function(data) {
  max(data) - min(data) 
}

dstats.variance <- function(data, ...) {
  var(data, ...)
}

dstats.std.dev <- function(data) {
  sd(data)
}

dstats.coef.var <- function(data) {
  (var(data) / mean(data)) * 100
}

dstats.std.error <- function(data) {
  sd(data) / sqrt(length(data))
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
  2 * dstats.std.error.skew(data) * sqrt((n^2 - 1) / ((n - 3)*(n + 5)))
}

dstats.test.kurtosis <- function(data) {
  dstats.kurtosis(data) / dstats.std.error.kurtosis(data)
}