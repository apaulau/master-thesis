# Descriptive statistics

# Function for getting all descriptive statistics
dstats.describe <- function(data, type="", locale=FALSE, shiny=FALSE) {
  cv <- dstats.coef.var(data)
  stats <- c(dstats.mean(data), dstats.median(data), dstats.quartile.lower(data),
             dstats.quartile.upper(data), dstats.min(data), dstats.max(data),
             dstats.range(data), dstats.quartile.range(data), dstats.variance(data),
             dstats.std.dev(data), if(!is.na(cv)){cv}, dstats.std.error(data),
             dstats.skew(data), dstats.std.error.skew(data), dstats.kurtosis(data),
             dstats.std.error.kurtosis(data))
  
  if(nchar(type)) {
    dstats.write(data=data, type=type) ## TODO: need to improve -- now it computes two times the same things
  }
  if (locale) {
    descr.row <- c("Среднее", "Медиана", "Нижний квартиль", "Верхний квартиль", 
                   "Минимум", "Максимум", "Размах", "Квартильный размах",
                   "Дисперсия", "Стандартное отклонение", if(!is.na(cv)) {"Коэффициент вариации"},
                   "Стандартная ошибка", "Асимметрия", "Ошибка асимметрии",
                   "Эксцесс", "Ошибка эксцесса")
    descr.col <- c("Значение")
  } else {
    descr.row <- c("Mean", "Median", "Lower Quartile", "Upper Quartile", "Range",
                   "Minimum", "Maximum", "Quartile Range", "Variance", "Standard Deviation", 
                   if (!is.na(cv)) {"Coefficient of Variance"}, "Standard Error", "Skewness", 
                   "Std. Error Skewness", "Kurtosis", "Std. Error Kurtosis")
    descr.col <- c("Value")
  }
  if (!shiny) {
    df <- data.frame(stats, row.names=descr.row) 
    colnames(df) <- descr.col
  } else {
    df <- data.frame(descr.row, sapply(stats, format, digits=2, scientific=FALSE, nsmall=1)) 
    colnames(df) <- c("Статистика", "Значение")
  }
  
  df
}

dstats.mean <- function(data, ...) {
  m <- mean(data, ...)
  if (m < .0000001) {
    m <- 0
  }
  m
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
  mn <- mean(data)
  if (abs(mn) > 1.987171e-15) {
    (var(data) / mean(data)) * 100
  } else
    NA
}

dstats.std.error <- function(data) {
  sd(data) / sqrt(length(data))
}

dstats.skew <- function(data) {
  n <- length(data)
  mean <- mean(data)
  (n * sum(sapply(data, FUN=function(x){(x - mean)^3}))) /
    ((n - 1) * (n - 2) * dstats.std.dev(data)^3)
}

dstats.std.error.skew <- function(data) {
  n <- length(data)
  sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
}

dstats.test.skew <- function(data) {
  dstats.skew(data) / dstats.std.error.skew(data)
}

dstats.kurtosis <- function(data) {
  n <- length(data)
  mean <- mean(data)
  (n * (n + 1) * sum(sapply(data, FUN=function(x){(x - mean)^4})) - 3 * (sum(sapply(data, FUN=function(x){(x - mean)^2})))^2 * (n - 1)) / 
    ((n - 1) * (n - 2) * (n - 3) * dstats.variance(data)^2)
}

dstats.std.error.kurtosis <- function(data) {
  n <- length(data)
  2 * dstats.std.error.skew(data) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
}

dstats.test.kurtosis <- function(data) {
  dstats.kurtosis(data) / dstats.std.error.kurtosis(data)
}

dstats.write <- function (data, type) {
  WriteDescriptiveStatistic(expression=dstats.mean(data), type=type, name="mean")
  WriteDescriptiveStatistic(expression=dstats.variance(data), type=type, name="variance")
  WriteDescriptiveStatistic(expression=paste(format(dstats.coef.var(data), nsmall=2, digits=4), "\\%"), type=type, name="coef-var")
  WriteDescriptiveStatistic(expression=dstats.skew(data), type=type, name="skew")
  WriteDescriptiveStatistic(expression=dstats.kurtosis(data), type=type, name="kurtosis")
  WriteDescriptiveStatistic(expression=dstats.test.skew(data), type=type, name="test-skew")
  WriteDescriptiveStatistic(expression=dstats.test.kurtosis(data), type=type, name="test-kurtosis")
}