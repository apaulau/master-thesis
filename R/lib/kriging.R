## Calculates kriging prediction based on passed variogram model
PredictWithKriging <- function (data, x, y=rep(1, kObservationNum), variogram_model, future=0) {
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data) = ~x+y
  
  new_data <- data.frame("X"=c((kObservationNum + 1):(src.nrows + future)), "Y"=rep(1, src.nrows - kObservationNum + future))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model, debug.level=0)
}

## Compares predictions based on trend and kriging with actual values
CrossPrediction <- function (temperature, years, trend, kriging, name="", future=0) {
  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
    "year"=GetPredictionYears(years, src.nrows, 0))
  
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  prediction.kriging <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  if (nchar(name)) {
    filename <- paste0("figures/variogram/", name, "-cross-prediction.png")
    plot.crossprediction <- ggplot() +
      geom_line(data=actual, aes(x=year, y=temperature, linetype="Наблюдение")) +
      geom_line(data=prediction.trend, aes(x=year, y=temperature, linetype="Прогноз(тренд)")) +
      geom_line(data=prediction.kriging, aes(x=year, y=temperature, linetype="Прогноз(кригинг)")) +       
      scale_linetype_manual(name="Lines", values=c("Наблюдение"="solid", "Прогноз(тренд)"="dashed", "Прогноз(кригинг)"="dotdash")) +
      scale_x_continuous(breaks=seq(min(actual$year), max(actual$year) + 5 + future, by=1)) + xlab("Год наблюдения") +
      scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, С") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(color="")
    ggsave(plot=plot.crossprediction, file=filename, width=7, height=4)
  }
  
  prediction.kriging$temperature[3:(src.nrows-kObservationNum)] - actual$temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

### Missed complete understanding of this functionality, because it aren't used in further work. Seems like it used only for selection best parameters.
### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
#### todo: simpify this function, split it to several less complex functions
ComparePredictionParameters <- function(data, trend, x, filename) {
  cutoffs <- c(1:kObservationNum)
  
  computePredictionResidual <- function(data, trend, variog=ComputeVariogram, cressie, x, cutoff) {
    variogram <- variog(data, x=x, cressie=cressie, cutoff=cutoff)
    kriging <- PredictWithKriging(data, x=x, variogram_model=variogram$var_model)
    residual <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging)
    return(residual)
  }
  
  manualResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, variog=ComputeManualVariogram, x=x, cressie=FALSE, cutoff=cutoff)))
  classicalResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, x=x, cressie=FALSE, cutoff=cutoff)))
  robustResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, x=x, cressie=TRUE, cutoff=cutoff)))
  
  plot.check <- ggplot() + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=manualResult), aes(x=X, y=Y, linetype="Фиксированная")) + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=classicalResult), aes(x=X, y=Y, linetype="Классическая")) + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=robustResult), aes(x=X, y=Y, linetype="Робастная")) + 
    scale_linetype_manual(name="Lines", values=c("Фиксированная"="solid", "Классическая"="dashed", "Робастная"="dotdash")) +
    scale_x_continuous(breaks=cutoffs) +
    xlab("Максимальное расстояние") + ylab("MSE") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
  ggsave(plot=plot.check, file=filename, width=7, height=4)
  
  list(manual=which.min(manualResult), classical=which.min(classicalResult), robust=which.min(robustResult))
}
