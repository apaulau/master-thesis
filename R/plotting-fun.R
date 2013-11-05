figure.ts <- function (data) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = "Temperature Time Series");
  lines(data, lwd = 1, col = "darkred");
}

figure.hist <- function (data) {
  hist(data$Temperature, col="lightgrey", xlim= c(14,27),
       freq=F, xlab="Temperature", main="Histogram of Temperature");
  curve(dnorm(x, mean=mean(data$Temperature), sd=sd(data$Temperature)),
        add=TRUE, col="darkgrey", lwd=2) 
}

figure.ggts <- function (data) {
  ggplot(data, aes(x=Date, y=Temperature)) + 
    geom_point(pch=21, col="darkgrey", bg="grey") + 
    geom_line(col="darkred") + 
    ggtitle("Temperature Time Series") + 
    theme_bw();
}

figure.gghist <- function (data) {
  ggplot(data, aes(x=Temperature)) + geom_histogram() + theme_bw();
}