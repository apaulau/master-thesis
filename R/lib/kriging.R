## Calculates kriging prediction based on passed variogram model
PredictWithKriging <- function (data, x, observations, variogram_model, nrows, future=0) {
  y <- rep(1, observations)
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data) = ~x+y
  
  new_data <- data.frame("X"=c((observations + 1):(nrows + future)), "Y"=rep(1, nrows - observations + future))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model, debug.level=0)
}

## Compares predictions based on trend and kriging with actual values
CrossPrediction <- function (temperature, years, trend, kriging, observations, nrows, name, future=0) {
  actual <- data.frame("temperature"=temperature[(observations - 1):nrows],
    "year"=GetPredictionYears(years, nrows, 0, observations))
  
  prediction.trend <- data.frame("temperature"=c(temperature[(observations - 1):observations], trend[(observations + 1):nrows]),
    "year"=GetPredictionYears(years, nrows, future, observations))
  
  prediction.kriging <- data.frame("temperature"=c(temperature[(observations - 1):observations], trend[(observations + 1):nrows] + kriging$var1.pred),
    "year"=GetPredictionYears(years, nrows, future, observations))
  
  plot.crossprediction <- DrawCrossPrediction(actual, prediction.trend, prediction.kriging, future)
  filename <- paste0("figures/variogram/", name, "-cross-prediction.png")
  ggsave(plot=plot.crossprediction, file=filename, width=7, height=3.3)
}

## Computes resdiuals after kriging prediction substract
ComputeKrigingResiduals <- function(temperature, trend, kriging, observations, nrows) {
  actual <- temperature[(observations + 1):nrows]
  prediction <- trend[(observations + 1):nrows] + kriging$var1.pred
    
  actual - prediction
}


### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
ComparePredictionParameters <- function(data, trend, x, filename="", observations, nrows, adapt=TRUE) {
  cutoffs <- c(1:observations)
  
  computePredictionEstimation <- function(data, trend, cressie, x, cutoff, observations) {
    variogram <- ComputeVariogram(data, x=x, cressie=cressie, cutoff=cutoff, observations=observations)
    if (adapt) {
      kriging <- PredictWithKriging(data, x=x, observations=observations, variogram_model=variogram$var_model, nrows=nrows)
      residual <- ComputeKrigingResiduals(src$temperature, trend, kriging, observations, nrows)
      estimation <- MSE(residual)
    } else {
      crv <- computeCV(data, variogram$var_model, observations, nfold=observations)
      estimation <- cor(crv$observed, crv$var1.pred)
    }
    return(estimation)
  }
  
  classicalResult <- sapply(cutoffs, FUN=function(cutoff) computePredictionEstimation(data=data, trend=trend, x=x, cressie=FALSE, cutoff=cutoff, observations=observations))
  robustResult <- sapply(cutoffs, FUN=function(cutoff) computePredictionEstimation(data=data, trend=trend, x=x, cressie=TRUE, cutoff=cutoff, observations=observations))
    
  if (nchar(filename)) {
    plot.check <- DrawParameterComparison(cutoffs, classicalResult, robustResult, adapt)
    ggsave(plot=plot.check, file=filename, width=7, height=3.3)
  }

  list(classical=which.min(classicalResult), robust=which.min(robustResult))
}


RunThroughParameters <- function(data, trend, x, filename="", observations, nrows, 
  fit=FALSE, cutoff, cressie, model, nugget, sill, range, min=.1, max=10, step=.1, adapt=TRUE) {
  
  computePredictionEstimation <- function(data, trend, cressie, x, cutoff, observations, model, nugget, sill, range, adapt) {
    variogram <- ComputeManualVariogram(data, x=x, cressie=cressie, cutoff=cutoff, observations=observations, 
      fit=fit, model=model, nugget=nugget, psill=sill, range=range)
    if (adapt) {
      kriging <- PredictWithKriging(data, x=x, observations=observations, variogram_model=variogram$var_model, nrows=nrows)
      residual <- ComputeKrigingResiduals(src$temperature, trend, kriging, observations, nrows)
      estimation <- MSE(residual)
    } else {
      crv <- computeCV(data, variogram$var_model, observations, nfold=observations)
      estimation <- cor(crv$observed, crv$var1.pred)
    }
    return(estimation)
  }
  
  params <- seq(min, max, step)
  if (is.na(cutoff)) {
    params <- 1:observations
    caption <- "Максимальное расстояние"
  } else if (is.na(nugget)) {
    caption <- "Наггет"
  } else if (is.na(sill)) {
    caption <- "Порог"
  } else if (is.na(range)) {    
    caption <- "Ранг"
  }

  result <- sapply(params, FUN=function(param) 
    computePredictionEstimation(data=data, trend=trend, x=x, adapt=adapt,
      cressie=cressie, observations=observations, model=model, 
      cutoff=ifelse(is.na(cutoff), param, cutoff), 
      nugget=ifelse(is.na(nugget), param, nugget), 
      sill=ifelse(is.na(sill), param, sill), 
      range=ifelse(is.na(range), param, range)))
  
  ggplot() + 
    geom_line(data=data.frame("X"=params, "Y"=result), aes(x=X, y=Y)) + 
    scale_x_continuous(breaks=params[seq(1, length(params), 4)]) +
    xlab(caption) + ylab(ifelse(adapt, "MSE", "Корреляция")) +
    theme(axis.text.x = element_text(angle=90, hjust=1))
}


RunThroughParametersSSerr <- function(data, trend, x, filename="", observations, nrows, 
  fit=FALSE, cutoff, cressie, model, sill, range, min=.1, max=10, step=.1) {
  
  ComputeSSerr <- function(data, cressie, x, cutoff, observations, fit, model, sill, range) {
    variogram <- ComputeManualVariogram(data, x=x, cressie=cressie, cutoff=cutoff, observations=observations, 
      fit=fit, model=model, psill=sill, range=range)
    
    return(variogram$SSerr)
  }
  
  params <- seq(min, max, step)
  if (is.na(cutoff)) {
    params <- 1:observations
    result <- sapply(params, FUN=function(param) 
      ComputeSSerr(data=data, x=x,
        cressie=cressie, observations=observations,
        fit=fit, model=model, cutoff=param, sill=sill, range=range))
    caption <- "Максимальное расстояние"
  } else if (is.na(sill)) {
    result <- sapply(params, FUN=function(param) 
      ComputeSSerr(data=data, x=x,
        cressie=cressie, observations=observations,
        fit=fit, model=model, cutoff=cutoff, sill=param, range=range))
    caption <- "Порог"
  } else if (is.na(range)) {    
    result <- sapply(params, FUN=function(param) 
      ComputeSSerr(data=data, x=x,
        cressie=cressie, observations=observations,
        fit=fit, model=model, cutoff=cutoff, sill=sill, range=param))
    caption <- "Ранг"
  }
  
  ggplot() + 
    geom_line(data=data.frame("X"=params, "Y"=result), aes(x=X, y=Y)) + 
    scale_x_continuous(breaks=params[seq(1, length(params), 2)]) +
    xlab(caption) + ylab("SSerr") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
}

# Leave-one-out cross validation
computeCV <- function (data, var_model, observations, nfold) {
  df <- MakeFakeSpatialData(x=1:observations, data=data, observations=observations)
  
  out <- krige.cv(data~1, df, var_model, nfold=nfold, verbose=FALSE)
  
  return(out)
}

compStat <- function(cv) {
  out = list()
  # mean error, ideally 0:
  out$mean_error = ifelse(mean(cv$residual) < 1*10^(-10), 0, mean(cv$residual))
  # mean error divided by the mean of the observed values, measure for how large the mean_error is in contrast to the mean of the dataset
  #out$me_mean = out$mean_error / mean(cv$observed)
  out$RSS <- RSS(cv$residual)
  out$bias <- mean(cv$observed) - mean(cv$residual)
  out$effectivity <- out$RSS / sum((cv$observed - mean(cv$observed))^2)
  # mean absolute error, ideally 0, less vulnerable to outliers
  out$MAE = MAE(cv$residual)
  # MSE, ideally small
  out$MSE = MSE(cv$residual)
  # Mean square normalized error, ideally close to 1
  out$MSNE = MSE(cv$zscore)
  # correlation observed and predicted, ideally 1
  out$cor_obspred = cor(cv$observed, cv$var1.pred)
  # correlation predicted and residual, ideally 0
  out$cor_predres = cor(cv$observed - cv$residual, cv$residual)
  # RMSE, ideally small
  out$RMSE = RMSE(cv$residual)
  # RMSE / sd(observed), measure for how much the residuals vary to the total variation in the dataset
  #out$RMSE_sd = out$RMSE / sd(cv$observed)
  # URMSE, ideally zero
  #out$URMSE = sqrt(out$MSE - mean(cv$residual)^2)
  # Inter quartile range, ideally small
  #out$iqr = IQR(cv$residual)
  return(out)
}
computeCVStatistics <- function(cv, digits=4){
  out <- compStat(cv)
  
  out = lapply(out, format, digits = digits)
  out = t(t(out))
  out <- data.frame(c("Среднее значение", "Сумма квадратов невязок", "Смещение", "Коэффицинт эффективности", "MAE", "MSE", "MSE(Z-значение)", "Корреляция(наблюдение, прогноз)", "Корреляция(прогноз, остатки)", "RMSE"), out)
  colnames(out) <- c("Статистика", "Значение")
  return(out)
}

computePlainStatistics <- function(cv, digits=4){
  out <- c(RSS(cv$residual), RSS(cv$residual) / sum((cv$observed - mean(cv$observed))^2), MAE(cv$residual), MSE(cv$residual), cor(cv$observed, cv$var1.pred))
  return(out)
}