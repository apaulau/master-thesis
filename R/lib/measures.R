RSS <- function (x) {
  sum(x^2)
}

MAE <- function (x) {
  mean(abs(x))
}

MSE <- function (x) {
  mean(x^2)
}

RMSE <- function (x) {
  sqrt(MSE(x))
}


