MAE <- function (x) {
  mean(abs(x))
}

MSE <- function (x) {
  mean(x^2)
}

RMSE <- function (x) {
  sqrt(MSE(x))
}


