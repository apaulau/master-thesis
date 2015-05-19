source("R/lib/afv.R")
source("R/lib/variogram.R")
source("R/lib/kriging.R")

## Function definition: need to be moved into isolated place
# Completes trend values up to source observation number
computeTrend <- function (fit, future=0) {
  c(sapply(c(1 : (nrows + future)), FUN=function(x) fit$coefficients[[1]] + x * fit$coefficients[[2]]))
}

# Computes prediction with passed parameters and saves all needed info and plots
processPrediction <- function (data, year, variog=ComputeVariogram, cressie, cutoff, name, caption) {
  variogram <- variog(data, x=ConvertYearsToNum(year), cressie=cressie, cutoff=cutoff, name=name, observations=kObservationNum)
  
  WriteCharacteristic(variogram$var_model[[2]][1], type="variogram", name=paste0(name, "-nug"))
  WriteCharacteristic(variogram$var_model[[2]][2], type="variogram", name=paste0(name, "-psill"))
  WriteCharacteristic(variogram$var_model[[3]][2], type="variogram", name=paste0(name, "-range"))
  
  prediction <- PredictWithKriging(data, x=ConvertYearsToNum(year), observations=kObservationNum, variogram_model=variogram$var_model, nrows=nrows)
  CrossPrediction(src$temperature, src$year, trend, prediction, name, observations=kObservationNum, nrows=nrows)
  residual <- ComputeKrigingResiduals(src$temperature, trend, prediction, observations=kObservationNum, nrows=nrows)
  mse <- MSE(residual)
  
  prediction.compare <- data.frame("Год"=src$year[(kObservationNum + 1):nrows],
    "Наблюдение"=src$temperature[(kObservationNum + 1):nrows],
    "Прогноз"=prediction$var1.pred+trend[(kObservationNum + 1):nrows],
    "Тренд"=trend[(kObservationNum + 1):nrows])
  print(xtable(prediction.compare, caption=caption, label=paste0("table:", name, "-prediction"), digits=c(0, 0, 3, 3, 3)),
    file=paste0("out/variogram/", name, "-prediction.tex"))
  
  WriteCharacteristic(mse, type="variogram", name=paste0(name, "-mse"))
  
  list(variogram=variogram, prediction=prediction, residual=residual, mse=mse)
}

trend <- computeTrend(sample.fit)
sample.residuals <- sample.fit$residuals

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value

# Draw H-Scatterplot
sample.hscat <- DrawHScatterplot(sample.residuals[1:kObservationNum], cutoff)

# Compute prediction manually with choosed model ("best" what i found)
manual <- processPrediction(data=sample.residuals, year=sample$year, variog=ComputeManualVariogram, cressie=FALSE, cutoff=cutoff, name="manual", caption="Прогноз (сферическая модель)")

# Compute prediction with auto fit model using classical estimation
classical <- processPrediction(data=sample.residuals, year=sample$year, cressie=FALSE, cutoff=cutoff, name="classical", caption="Прогноз (классическая оценка)")

# Compute prediction with auto fit model using robust (cressie) estimation
robust <- processPrediction(data=sample.residuals, year=sample$year, cressie=TRUE, cutoff=cutoff, name="robust", caption="Прогноз (робастная оценка)")

models.comparison <- CompareClassicalModels(manual$variogram, classical$variogram, filename="figures/variogram/models-comparison.png")

# Find best cutoff parameters
cutoff <- ComparePredictionParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/parameter-comparison.png", observations=kObservationNum, nrows=nrows)

manual.best    <- processPrediction(data=sample.residuals, year=sample$year, variog=ComputeManualVariogram, cressie=FALSE, cutoff=cutoff$manual, name="manual-best", caption="Наилучший прогноз (сферическая модель)")
classcial.best <- processPrediction(data=sample.residuals, year=sample$year, cressie=FALSE, cutoff=cutoff$classical, name="classical-best", caption="Наилучший прогноз (классическая оценка)")
robust.best    <- processPrediction(data=sample.residuals, year=sample$year, cressie=TRUE, cutoff=cutoff$robust, name="robust-best", caption="Наилучший прогноз (робастная оценка)")