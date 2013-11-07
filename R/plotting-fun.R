figure.ts <- function (data) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = "Temperature Time Series");
  lines(data, lwd = 1, col = "darkred");
}

figure.hist <- function (data, title="Histogram", freq=T, dfun=dnorm, offset = 2) {
  min_x = min(data);
  max_x = max(data);
  hist(data, col="lightgrey", border="darkgrey", xlim= c(min_x - offset, max_x + offset),
       freq=freq, xlab="Temperature", main=title);
  if (!freq) {
    curve(dfun(x, mean=mean(data), sd=sd(data)),
          add=TRUE, col="darkred", lwd=2) 
  }
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