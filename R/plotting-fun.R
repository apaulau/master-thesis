figure.ts <- function(data, title="Temperature Time Series", ...) {
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xlab="Observation", xaxt="n", ...);
  lines(data, lwd = 1, col = "darkred");
  axis(side=1, at=0:length(data), labels=0:length(data), cex.axis=1, tck=-.05, las=1)
}

figure.ts2 <- function(data, title="Temperature Time Series with regression line", offset=2, ...) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- date.year.subtract(min(Date), 2)
  max_date <- date.year.add(max(Date), 2)
  
  plot(x=Date, y=Temperature, main = title, xaxt="n", xlab="", pch=20, cex=.4,
       ylim=c(min_y - offset, max_y + offset), 
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")))
  
  legend(x=min_date, 26, c("Time plot", "Trend"), col = c(1,2),
         text.col = "black", merge = TRUE, lty=c(1,1))
  
  mtext("Date",side=1,line=4)
  lines(data, lwd = 1);
  abline(lm(Temperature ~ Date), col="red", lend=0, ljoin=0) # regression line (y~x)
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y/%m/%d"), cex.axis = .7, las=2, hadj=1)  
}

figure.residuals <- function(date, residuals, title="Detrended time series", offset=2, ...) {
  min_date <- date.year.subtract(min(date), 2)
  max_date <- date.year.add(max(date), 2)
  
  plot(x=Date, y=residuals, type='l', main = title, xaxt="n", xlab="Date", 
       ylab="Residuals", pch=20,
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")))
  
  abline(a=0, b=0, col=2, lwd=2)
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)  
}

figure.hist <- function(data, title="Histogram", freq=T, dfun=dnorm, offset = 2) {
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
  
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)
}

figure.qqnorm <- function(data, title="Normal Q-Q Plot for Temperature", ...) {
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