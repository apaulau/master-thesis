figure.ts <- function (data, title="Temperature Time Series", ...) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xaxt="n", ...);
  lines(data, lwd = 1, col = "darkred");
  axis(side=1, at=0:length(data), labels=0:length(data), cex.axis=1, tck=-.05, las=1)
}

figure.hist <- function (data, title="Histogram", freq=T, dfun=dnorm, offset = 2) {
  min_x <- min(data);
  max_x <- max(data);
  hist(data, col="lightgrey", border="darkgrey", xlim=c(min_x - offset, max_x + offset),
       freq=freq, xlab="Temperature", main=title);
  if (!freq) {
    curve(dfun(x, mean=mean(data), sd=sd(data)),
          add=TRUE, col="darkred", lwd=2) 
  }
}

figure.bagplot <- function(data, title="Bagplot", offset = 2) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- min(Date)
  max_date <- max(Date)
  YEAR=365
  
  bagplot(x=Date, y=Temperature, xaxt="n", main=title, xlab="Date", ylab="Temperature",
          show.whiskers=F, ylim=c(min_y - offset, max_y + offset), 
          xlim=c(as.Date(min_date - 5*YEAR, "%Y"), as.Date(max_date + 2*YEAR, "%Y")))
  
  Date <- append(seq.Date(min_date-5*YEAR, min_date, "years"), Date)
  Date <- append(Date, seq.Date(max_date, max_date, "years"))
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)
}

figure.qqnorm <- function(data, title="Normal Q-Q Plot of Temperature", ...) {
  qqnorm(data, main=title, pch = 21, col = 'darkgrey', bg = 'grey', ...)
  qqline(data, col="darkred")
}

figure.ggts <- function (data) {
  ggplot(data, aes(x=Date, y=Temperature)) + 
    geom_point(pch=21, col="darkgrey", bg="grey") + 
    geom_line(col="darkred") + 
    ggtitle("Temperature Time Series") + 
    theme_bw()
}

figure.gghist <- function (data) {
  ggplot(data, aes(x=Temperature)) + geom_histogram() + theme_bw()
}