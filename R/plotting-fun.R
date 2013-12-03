source("R/misc-fun.R")

figure.ts <- function (data, title="Temperature Time Series", ...) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xlab="Observation", xaxt="n", ...);
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
  min_date <- date.year.subtract(min(Date), 5)
  max_date <- date.year.add(max(Date), 2)
  
  bagplot(x=Date, y=Temperature, xaxt="n", main=title, xlab="Date", ylab="Temperature",
          show.whiskers=F, ylim=c(min_y - offset, max_y + offset), 
          xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")),
          cex=.5, transparency=T)
  
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)
}

figure.scatterplot <- function(data, title="Scatterplot", offset = 2, ...) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- date.year.subtract(min(Date), 5)
  max_date <- date.year.add(max(Date), 2)
    
  plot(x=Date, y=Temperature, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xaxt="n",
       ylim=c(min_y - offset, max_y + offset), 
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y"), ...))
  
  abline(lm(Temperature ~ Date), col="red") # regression line (y~x)
  lines(lowess(Date, Temperature), col="blue") # lowess line (x,y) 
  
  Date <- date.wrap(min_date, Date, max_date)
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