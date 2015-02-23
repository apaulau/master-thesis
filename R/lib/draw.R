DrawDataRepresentation <- function (data, filename="", datebreaks) {
  plot.source <- ggplot(data, aes(x=year, y=temperature)) + 
    geom_point() + geom_line() + 
    scale_x_continuous(breaks=datebreaks) + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    xlab("Год наблюдения") + ylab("Температура, ºС")
  
  if (nchar(filename)) {
    plot.save(plot.source, filename=filename)
  }
  
  plot.source
}

# plot is based on some magic; investigate again what is this magic is // TODO: look up can I introduce new variable with y-breaks
DrawScatterPlot <- function (data, filename="", datebreaks) {
  plot.scatter <- ggplot(data, aes(x=year, y=temperature)) + 
    geom_point() + geom_abline(intercept=-194.632277, slope=.107706, color="blue") +
    scale_x_continuous(breaks=datebreaks) + 
    scale_y_continuous(breaks=seq(10, 30, 1), limits=c(14, 26)) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
  
  if (nchar(filename)) {
    plot.save(plot.scatter, filename=filename)
  }
  
  plot.scatter
}

DrawHistogram <- function (data, filename="", binwidth=1.2) {
  plot.hist <- ggplot(data, aes(x=temperature), geom='blank') +   
    geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=binwidth, alpha=.6) +
    stat_function(fun=dnorm, colour='red', arg=list(mean=mean(data$temperature), sd=sd(data$temperature))) +    
    labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
  
  if (nchar(filename)) {
    plot.save(plot.hist, filename=filename)
  }
  
  plot.hist
}

DrawQuantileQunatile <- function (data, filename="") {
  plot.qq <- ggqqp(data)
  
  if (nchar(filename)) {
    plot.save(plot.qq, filename=filename)
  }
  
  plot.qq
}

DrawTimeSeries  <- function (data, filename="", datebreaks) {
  plot.ts <- ggplot(data, aes(x=year, y=temperature)) + 
    geom_point() + geom_line() + stat_smooth(method=lm, se=FALSE) + 
    scale_x_continuous(breaks=datebreaks) + scale_y_continuous(breaks=seq(16, 28, 1)) + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
  
  if (nchar(filename)) {
    plot.save(plot.ts, filename=filename)
  }
  
  plot.ts
}

DrawAutoCorrelationFunction <- function (data, filename="") {
  plot.acf <- ggacf(data)
  
  if (nchar(filename)) {
    plot.save(plot.acf, filename=filename)
  }
  
  plot.acf
}