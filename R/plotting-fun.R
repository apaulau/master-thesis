figure.ts <- function (data) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = "Temperature Time Series");
  lines(data, lwd = 1, col = "darkred");
}