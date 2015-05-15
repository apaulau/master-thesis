library(ggvis)
library(dplyr)
library(tseries)
library(sp)
library(gstat)
library(reshape2)

# Just example that it works
cv.example <- computeCV(c(1,2,3,4,5,6,7,8), 8, ComputeVariogram(data=c(1,2,3,4,5,6,7,8), x=c(1:8), cressie=FALSE, cutoff=6, width=FALSE)$var_model)

cv.manual <- computeCV(research.data.residuals, 32, variogram.manual$var_model)
cv.robust <- computeCV(research.data.residuals, 32, variogram.robust$var_model)
cv.robust.best <- computeCV(research.data.residuals, 32, variogram.robust.best$var_model)

computeCVStatistics <- function(cv, digits=4){
  out = list()
  # mean error, ideally 0:
  out$mean_error = mean(cv$residual)
  # mean error divided by the mean of the observed values, measure for how large the mean_error is in contrast to the mean of the dataset
  out$me_mean = out$mean_error / mean(cv$observed)
  # mean absolute error, ideally 0, less vulnerable to outliers
  out$MAE = mean(abs(cv$residual))
  # MSE, ideally small
  out$MSE = mean(cv$residual^2)
  # Mean square normalized error, ideally close to 1
  out$MSNE = mean(cv$zscore^2)
  # correlation observed and predicted, ideally 1
  out$cor_obspred = cor(cv$observed, cv$observed - cv$residual)
  # correlation predicted and residual, ideally 0
  out$cor_predres = cor(cv$observed - cv$residual, cv$residual)
  # RMSE, ideally small
  out$RMSE = sqrt(sum(cv$residual^2) / length(cv$residual))
  # RMSE / sd(observed), measure for how much the residuals vary to the total variation in the dataset
  out$RMSE_sd = out$RMSE / sd(cv$observed)
  # URMSE, ideally zero
  out$URMSE = sqrt((sum(cv$residual^2) / length(cv$residual)) - mean(cv$residual)^2)
  # Inter quartile range, ideally small
  out$iqr = IQR(cv$residual)
  
  out = lapply(out, signif, digits = digits)
  out = t(t(out))
  colnames(out) <- c("Value")
  return(out)
}

# Leave-one-out cross validation
computeCV <- function (data, length, var_model) {
  df <- data.frame("data"=data, "x"=c(1:length), "y"=rep(1, length))
  coordinates(df) <- ~x+y
  
  out <- krige.cv(data~1, df, var_model, nfold=length)
  
  return(out)
}