## Calculates kriging prediction based on passed variogram model
PredictWithKriging <- function (data, x, observations, variogram_model, future=0) {
  y <- rep(1, observations)
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data) = ~x+y
  
  new_data <- data.frame("X"=c((observations + 1):(src.nrows + future)), "Y"=rep(1, src.nrows - observations + future))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model, debug.level=0)
}

## Compares predictions based on trend and kriging with actual values
CrossPrediction <- function (temperature, years, trend, kriging, name="", future=0) {
  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
    "year"=GetPredictionYears(years, src.nrows, 0, kObservationNum))
  
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
    "year"=GetPredictionYears(years, src.nrows, future, kObservationNum))
  
  prediction.kriging <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future, kObservationNum))
  
  if (nchar(name)) {
    filename <- paste0("figures/variogram/", name, "-cross-prediction.png")
    plot.crossprediction <- DrawCrossPrediction(actual, prediction.trend, prediction.kriging, future)
    ggsave(plot=plot.crossprediction, file=filename, width=7, height=4)
  }
  
  prediction.kriging$temperature[3:(src.nrows-kObservationNum)] - actual$temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

### Missed complete understanding of this functionality, because it aren't used in further work. Seems like it used only for selection best parameters.
### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
#### todo: simpify this function, split it to several less complex functions
ComparePredictionParameters <- function(data, trend, x, filename="") {
  cutoffs <- c(1:kObservationNum)
  
  computePredictionResidual <- function(data, trend, variog=ComputeVariogram, cressie, x, cutoff) {
    variogram <- variog(data, x=x, cressie=cressie, cutoff=cutoff)
    kriging <- PredictWithKriging(data, x=x, observations=kObservationNum, variogram_model=variogram$var_model)
    residual <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging)
    return(residual)
  }
  
  manualResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, variog=ComputeManualVariogram, x=x, cressie=FALSE, cutoff=cutoff)))
  classicalResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, x=x, cressie=FALSE, cutoff=cutoff)))
  robustResult <- sapply(cutoffs, FUN=function(cutoff) MSE(computePredictionResidual(data=data, trend=trend, x=x, cressie=TRUE, cutoff=cutoff)))
  
  if (nchar(filename)) {
    plot.check <- DrawParameterComparison(cutoffs, manualResult, classicalResult, robustResult)
    ggsave(plot=plot.check, file=filename, width=7, height=4)
  }

  list(manual=which.min(manualResult), classical=which.min(classicalResult), robust=which.min(robustResult))
}