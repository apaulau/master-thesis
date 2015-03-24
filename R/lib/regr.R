regr.significance.Student <- function (y, alpha=.05) {
  # Makes conclusion.
  # coeff - simple letter for output
  MakeConclusion <- function(statistic, critical, coeff) {
    if (abs(statistic) > critical) {
      conclusion <- paste("нулевая гипотеза отвергается и отклонение", coeff, "от нуля носит неслучайный характер, и, следовательно, величина", coeff, "значима.", sep=" ")
    } else {
      conclusion <- paste("для параметра", coeff, "нельзя отвергнуть нулевую гипотезу", sep=" ")
    }
    conclusion
  }
  
  StudentCriticalPoint <- function(alpha, df) {
    qt(1 - alpha, df)
  }
  
  n <- length(y)
  x <- seq(1, n)
  fit <- lm(y ~ x)
  a <- fit$coefficients[[2]]
  b <- fit$coefficients[[1]]
  
  vardev <- var(y) * (1 - cor(y, x)^2)
  errorA <- sqrt(vardev) / (sqrt(var(x)) * sqrt(n - 2))
  errorB <- sqrt(vardev) / sqrt(n - 2) * sqrt(1 + mean(x)^2 / var(x))
  
  studentA <- a / errorA
  studentB <- b / errorB
  
  criticalPoint <- StudentCriticalPoint(alpha, n - 2)
  
  list(vardev=vardev, errors=c(errorA, errorB), coeff=c(a, b), statistic=c(studentA, studentB), critical=criticalPoint,
    conclusion=c(MakeConclusion(studentA, criticalPoint, "$ a $"), MakeConclusion(studentB, criticalPoint, "$ b $")))
}

regr.adequacy <- function(y, alpha=.05) {
  regression <- function(x, a, b) a * x + b

  CriticalPoint <- function(alpha, df1, df2) {
    qf(1 - alpha, df1, df2)
  }
  
  MakeConclusion <- function(statistic, critical) {
    if (statistic > critical) {
      conclusion <- "нулевая гипотеза о равенстве дисперсий отвергается, что означает в рассматриваемом случае адекватность регрессионной модели"
    } else {
      conclusion <- "нельзя отвергнуть нулевую гипотезу о равенстве дисперсий"
    }
    conclusion
  }

  n <- length(y)
  x <- seq(1, n)
  fit <- lm(y ~ x)
  a <- fit$coefficients[[2]]
  b <- fit$coefficients[[1]]
  y_ <- sapply(X=x, FUN=regression, a=a, b=b)
  
  var_ <- 1 / n * sum(sapply(x, FUN=function(j){(y_[j] - mean(y))^2}))
  determination <- var_ / var(y)
  
  linearity <- determination - cor(y, x)^2
  
  resvar <- 1 / n * sum(sapply(x, FUN=function(j){(y[j] - y_[j])^2}))
  
  statistic <- (n - 2) * var_ / resvar
  critical <- CriticalPoint(alpha, 1, n - 2)
  
  list(modvar=var_, determination=determination, linearity=linearity, 
    Fisher=list(resvar=resvar, statistic=statistic, critical=critical, conclusion=MakeConclusion(statistic, critical)))
}