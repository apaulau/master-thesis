## Calculates kriging prediction based on passed varigram model
PredictWithKriging <- function (data, x, y=rep(1, kObservationNum), variogram_model, future=0) {
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data) = ~x+y
  
  new_data <- data.frame("X"=c((kObservationNum + 1):(src.nrows + future)), "Y"=rep(1, src.nrows - kObservationNum + future))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model)
}

## Compares predictions based on trend and kriging with actual values
CrossPrediction <- function (temperature, years, trend, kriging, file_prediction="", future=0) {
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  prediction.kriging <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
    "year"=GetPredictionYears(years, src.nrows, 0))
  
  if (nchar(file_prediction)) {
    plot.crossprediction <- ggplot() +
      geom_line(data=prediction.kriging, aes(x=year, y=temperature, linetype="Прогноз Кригинг")) + 
      geom_line(data=prediction.trend, aes(x=year, y=temperature, linetype="Прогноз Тренд")) +
      geom_line(data=actual, aes(x=year, y=temperature, linetype="Актуальное")) +
      labs(color="") +
      scale_linetype_manual(name="Lines", values=c("Прогноз Кригинг"="dashed", "Прогноз Тренд"="dotdash", "Актуальное"="solid")) +
      scale_x_continuous(breaks=seq(min(actual$year), max(actual$year) + 5 + future, by=1)) + xlab("Год наблюдения") +
      scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, С") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(color="")
    ggsave(plot=plot.crossprediction, file=file_prediction, width=7, height=4)
  }
  
  prediction.kriging$temperature[3:(src.nrows-kObservationNum)] - actual$temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

### Missed complete understanding of this functionality, because it aren't used in further work. Seems like it used only for selection best parameters.
### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
#### todo: simpify this function, split it to several less complex functions
ComparePredictionParameters <- function (data, trend, x, y=rep(1, kObservationNum), width=1, filename) {
  lens <- 1:kObservationNum
  manualResult <- c()
  classicalResult <- c()
  robustResult <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  i <- 1
  for(l in lens) {
    variogram.manual    = ComputeManualVariogram(data, cutoff=l)
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=FALSE, width=width)
    variogram.robust    = autofitVariogram(data~1, spdata, cutoff=l, cressie=TRUE, width=width)
    
    kriging.manual    <- PredictWithKriging(data, x=x, variogram_model=variogram.manual$var_model)
    kriging.classical <- PredictWithKriging(data, x=x, variogram_model=variogram.classical$var_model)
    kriging.robust    <- PredictWithKriging(data, x=x, variogram_model=variogram.robust$var_model)
    
    res.manual    <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging.manual)
    res.classical <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging.classical)
    res.robust    <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging.robust)
    
    manualResult[i]    <- MSE(e=res.manual)
    classicalResult[i] <- MSE(e=res.classical)
    robustResult[i]    <- MSE(e=res.robust)
    i = i + 1 ## todo: find out how to avoid this construction
  }
  
  plot.check <- ggplot() + 
    geom_line(data=data.frame("X"=lens, "Y"=manualResult), aes(x=X,y=Y)) + 
    geom_line(data=data.frame("X"=lens, "Y"=classicalResult), aes(x=X,y=Y), linetype="dashed") + 
    geom_line(data=data.frame("X"=lens, "Y"=robustResult), aes(x=X,y=Y), linetype="dotdash") + 
    scale_x_continuous(breaks=lens) 
  ggsave(plot=plot.check, file=filename, width=7, height=4)
}
