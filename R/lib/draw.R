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
    geom_point() + geom_smooth(method="lm", se=FALSE, fullrange=TRUE) +
    scale_x_continuous(breaks=datebreaks, limits=c(min(datebreaks), max(datebreaks))) + 
    scale_y_continuous(breaks=seq(10, 30, 1), limits=c(14, 26)) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
  
  if (nchar(filename)) {
    plot.save(plot.scatter, filename=filename)
  }
  
  plot.scatter
}

DrawHistogram <- function (data, filename="", binwidth=1.2, fit=TRUE) {
  plot.hist <- ggplot(data, aes(x=temperature), geom='blank') +   
    geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=binwidth, alpha=.6) +
    labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
  
  if (fit) {
    plot.hist <- plot.hist + stat_function(fun=dnorm, colour="#D55E00", arg=list(mean=mean(data$temperature), sd=sd(data$temperature)))
  }
  
  if (nchar(filename)) {
    plot.save(plot.hist, filename=filename)
  }
  
  plot.hist
}

DrawQuantileQuantile <- function (data, filename="") {
  plot.qq <- ggqqp(data)
  
  if (nchar(filename)) {
    plot.save(plot.qq, filename=filename)
  }
  
  plot.qq
}

DrawTimeSeries  <- function (data, filename="", datebreaks) {
  plot.ts <- ggplot(data, aes(x=year, y=temperature)) + 
    geom_point() + geom_line() + stat_smooth(method=lm, se=FALSE) + 
    scale_x_continuous(breaks=datebreaks) + 
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

DrawHScatterplot <- function (data) {  
  # Make fake second coordinate
  p <- data.frame("X"=c(1:kObservationNum), "Y"=rep(1, kObservationNum))
  # Calculate distances
  p.dist<-as.matrix(dist(p[,c("X", "Y")]))
  dist.breaks<-quantile(p.dist,seq(.1, .9, by=.1))
  coordinates(p) <- ~ X + Y
  
  Residuals <- data
  hsc <- hscat(Residuals~1, p, breaks=0:20)
  
  pdf("figures/residual/hscat.pdf", width=7, height=3.3)
  print(hsc)
  dev.off()
  
  hsc
}

DrawCrossPrediction <- function (actual, trend, kriging, future) {
  ggplot() + geom_line(data=actual, aes(x=year, y=temperature, linetype="Наблюдение")) +
    geom_line(data=kriging, aes(x=year, y=temperature, linetype="Прогноз")) +       
    geom_line(data=trend, aes(x=year, y=temperature, linetype="Тренд")) +
    scale_linetype_manual(name="Lines", values=c("Наблюдение"="solid", "Прогноз"="dotdash", "Тренд"="dashed"), 
      labels=c(expression(X(t)), expression(X^{"*"}*(t)), expression(y(t)))) +
    scale_x_continuous(breaks=seq(min(actual$year), max(actual$year) + 5 + future, by=1)) + xlab("Год наблюдения") +
    scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, ºС") +
    theme(legend.title=element_blank()) +
    labs(color="")
}

DrawParameterComparison <- function(cutoffs, classical, robust, adapt) {
  ggplot() + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=classical), aes(x=X, y=Y, linetype="Матерона")) + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=robust), aes(x=X, y=Y, linetype="Кресси-Хокинса")) + 
    scale_linetype_manual(name="Оценка", values=c("Матерона"="solid", "Кресси-Хокинса"="dashed")) +
    scale_x_continuous(breaks=cutoffs) +
    xlab("Максимальное расстояние") + ylab(ifelse(adapt, "MSE", "Корреляция")) +
    theme(axis.text.x = element_text(angle=90, hjust=1))
}