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

## Acf plot
ggacf <- function (data, ci=.95, lag.max=30, xlab="Лаг", ylab="Автокорреляция") {
  ## Auto Correlation Function computation and definition
  acf    <- acf(data, plot=FALSE, lag.max=lag.max)
  acfdf  <- data.frame(acf=acf$acf, lag=acf$lag) # data frame for computed ACF
  clim   <- qnorm((1 + ci) / 2) / sqrt(acf$n.used) # limit // TODO: find out where it from / or remember what is it for

  ggplot(data=acfdf, mapping=aes(x=lag, y=acf)) +
    geom_hline(yintercept=c(-clim, clim), linetype="dashed", col="blue") +
    geom_segment(mapping=aes(xend=lag, yend=0)) +
    labs(color="") + xlab(xlab) + ylab(ylab)
}

## ggplot wrapper for saving plots.
plot.save <- function(plot, filename, path="figures", width=7, height=3.3, ...) {
  filepath <- paste(path, filename, sep="/")
  ggsave(plot=plot, file=filepath, width=width, height=height, ...)
}
