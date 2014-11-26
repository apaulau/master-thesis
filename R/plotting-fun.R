figure.bagplot <- function(data, title="Bagplot", offset = 2, xlab, ylab) {
  Date <- data$year
  Temperature <- data$temperature
  min_y <- min(Temperature)
  max_y <- max(Temperature)
  min_date <- min(Date) - 5
  max_date <- max(Date) + 2
  
  par(mar=c(4.1,4.1,.1,.1))
  bagplot(x=Date, y=Temperature, xaxt="n", main=title, xlab=xlab, ylab=ylab,
          show.whiskers=FALSE, ylim=c(min_y - offset, max_y + offset), 
          xlim=c(min_date, max_date),
          cex=.5, transparency=TRUE)
  
  Date <- c(min_date:min(Date), Date, max(Date):max_date)
  axis(1, Date, cex.axis=.7)
}

figure.acf  <- function(data, ...) {
  par(mar=c(4.1, 4.1, .1, .1))
  acf(data, lag.max=length(data) - 5, main="", xlab="Лаг", ylab="Автокорреляция", ...)
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
  y <- quantile(vec[!is.na(vec)], c(.25, .75))
  x <- qnorm(c(.25, .75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    xlab("Теоретические квантили") + ylab("Выборочные квантили")
}