library(ggvis)
library(dplyr)
library(tseries)
library(sp)
library(gstat)
library(reshape2)

# Just example that it works
vex <- ComputeVariogram(data=c(1,2,3,4,5,6,7,8), x=c(1:8), cressie=FALSE, cutoff=6, observations=8)
kex <- PredictWithKriging(c(1,2,3,4,5,6,7,8), x=c(1:8), observations=8, variogram_model = vex$var_model, nrows=8, pred=c(4))
cv.example <- computeCV(c(1,2,3,4,5,6,7,8), 8, vex$var_model)

cv.manual <- computeCV(sample.residuals, 32, variogram.manual$var_model)
cv.robust <- computeCV(sample.residuals, 32, variogram.robust$var_model)

fit = lm(src$temperature[1:32] ~ c(1:32))
res <- fit$residuals #sapply(1:32, FUN=function(i) fit$residuals[[i]])
variogram <- vgm("Lin", nugget=0.3, range=6, psill=2)
kr <- PredictWithKriging(sample.residuals, x=ConvertYearsToNum(src$year), observation=32, variogram_model = variogram$var_model, nrows=38)
cvb <- computeCV(res, 32, variogram$var_model)

computeCVStatistics <- function(cv, digits=4){
  out = list()
  # mean error, ideally 0:
  out$mean_error = mean(cv$residual)
  # mean error divided by the mean of the observed values, measure for how large the mean_error is in contrast to the mean of the dataset
  out$me_mean = out$mean_error / mean(cv$observed)
  # mean absolute error, ideally 0, less vulnerable to outliers
  out$MAE = MAE(cv$residual)
  # MSE, ideally small
  out$MSE = MSE(cv$residual)
  # Mean square normalized error, ideally close to 1
  out$MSNE = MSE(cv$zscore)
  # correlation observed and predicted, ideally 1
  out$cor_obspred = cor(cv$observed, cv$observed - cv$residual)
  # correlation predicted and residual, ideally 0
  out$cor_predres = cor(cv$observed - cv$residual, cv$residual)
  # RMSE, ideally small
  out$RMSE = RMSE(cv$residual)
  # RMSE / sd(observed), measure for how much the residuals vary to the total variation in the dataset
  out$RMSE_sd = out$RMSE / sd(cv$observed)
  # URMSE, ideally zero
  out$URMSE = sqrt(out$MSE - mean(cv$residual)^2)
  # Inter quartile range, ideally small
  out$iqr = IQR(cv$residual)
  
  out = lapply(out, signif, digits = digits)
  out = t(t(out))
  colnames(out) <- c("Value")
  return(out)
}

# Leave-one-out cross validation
computeCV <- function (data, length, var_model) {
  df <- data.frame(data=data, "x"=c(1:length), "y"=rep(1, length))
  coordinates(df) <- ~x+y
  
  out <- krige.cv(data~1, df, var_model, nfold=length)
  
  return(out)
}

#Randomly shuffle the data
yourData<-yourData[sample(nrow(yourData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use test and train data partitions however you desire...
}