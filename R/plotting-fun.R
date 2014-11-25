figure.ts <- function(data, title="Temperature Time Series", ...) {
  par(mar=c(4.1,4.1,.1,.1))
  plot(data, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xaxt="n", ...);
  lines(data, lwd = 1, col = "darkred");
  axis(side=1, at=0:length(data), labels=0:length(data), cex.axis=1, tck=-.05, las=1)
}

figure.ts2 <- function(data, title="Temperature Time Series with regression line", offset=2, xlab, ylab, ...) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- date.year.subtract(min(Date), 2)
  max_date <- date.year.add(max(Date), 2)
  
  par(mar=c(6.1,4.1,.1,.1))
  plot(x=Date, y=Temperature, main = title, xaxt="n", xlab="", ylab=ylab, pch=20, cex=.4,
       ylim=c(min_y - offset, max_y + offset), 
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")))
  
  mtext("Дата",side=1,line=4)
  lines(data, lwd = 1);
  abline(lm(Temperature ~ Date), col="red", lend=0, ljoin=0) # regression line (y~x)
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y/%m/%d"), cex.axis = .7, las=2, hadj=1)  
}

figure.residuals <- function(date, residuals, title="Detrended time series", offset=2, ...) {
  min_date <- date.year.subtract(min(date), 2)
  max_date <- date.year.add(max(date), 2)
  
  par(mar=c(4.1,4.1,.1,.1))
  plot(x=Date, y=residuals, type='l', main = title, xaxt="n", pch=20,
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")), ...)
  
  abline(a=0, b=0, col=2, lwd=2)
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)  
}

figure.hist <- function(data, title="Histogram", freq=T, dfun=dnorm, offset = 2, xlab, ylab) {
  min_x <- min(data);
  max_x <- max(data);
  
  par(mar=c(4.1,4.1,.1,.1))
  hist(data, col="lightgrey", border="darkgrey", xlim=c(min_x - offset, max_x + offset),
       freq=freq, main=title, xlab=xlab, ylab=ylab);
  if (!freq) {
    curve(dfun(x, mean=mean(data), sd=sd(data)),
          add=TRUE, col="darkred", lwd=2) 
  }
}

figure.bagplot <- function(data, title="Bagplot", offset = 2, xlab, ylab) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- min(Date) - 5
  max_date <- max(Date) + 2
  
  par(mar=c(4.1,4.1,.1,.1))
  bagplot(x=Date, y=Temperature, xaxt="n", main=title, xlab=xlab, ylab=ylab,
          show.whiskers=F, ylim=c(min_y - offset, max_y + offset), 
          xlim=c(min_date, max_date),
          cex=.5, transparency=T)
  
  Date <- c(min_date:min(Date), Date, max(Date):max_date)
  axis(1, Date, cex.axis = .7)
}

figure.scatterplot <- function(data, title="Scatterplot", offset = 2, ...) {
  Date <- data$Date
  Temperature <- data$Temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- date.year.subtract(min(Date), 5)
  max_date <- date.year.add(max(Date), 2)
  
  par(mar=c(4.1,4.1,.1,.1))
  plot(x=Date, y=Temperature, pch = 21, col = 'darkgrey', bg = 'grey', main = title, xaxt="n",
       ylim=c(min_y - offset, max_y + offset), 
       xlim=c(as.Date(min_date, "%Y"), as.Date(max_date, "%Y")), ...)
  
  abline(lm(Temperature ~ Date), col="red") # regression line (y~x)
  
  Date <- date.wrap(min_date, Date, max_date)
  axis(1, Date, format(Date, "%Y"), cex.axis = .7)
}

figure.qqnorm <- function(data, title="Normal Q-Q Plot for Temperature", ...) {
  par(mar=c(4.1,4.1,.1,.1))
  qqnorm(data, main=title, pch = 21, col = 'darkgrey', bg = 'grey', ...)
  qqline(data, col="darkred")
}

figure.acf  <- function(data, ...) {
  par(mar=c(4.1,4.1,.1,.1))
  acf(data, lag.max=length(data)-5, main="", xlab="Лаг", ylab="Автокорреляция", ...)
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

## Q-Q plot
ggqqp <- function (vec) {
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    xlab("Теоретические квантили") + ylab("Выборочные квантили")
}