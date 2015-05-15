source("R/lib/afv.R")
source("R/lib/variogram.R")
source("R/lib/kriging.R")

## Function definition: need to be moved into isolated place
### Just definition of mean standard error // TODO: find out exact formula and describe each parameter
MSE <- function (e, N=1) {
  sum(sapply(X=e, FUN=function(x) x**2)) / length(e)
}

# Completes trend values up to source observation number
computeTrend <- function (fit, future=0) {
  c(sapply(c(1 : (src.nrows + future)), FUN=function(x) fit$coefficients[[1]] + x * fit$coefficients[[2]]))
}

# Computes prediction with passed parameters and saves all needed info and plots
processPrediction <- function (data, year, variog=ComputeVariogram, cressie, cutoff, name, caption) {
  variogram <- variog(data, x=ConvertYearsToNum(year), cressie=cressie, cutoff=cutoff, name=name)
  
  WriteCharacteristic(variogram$var_model[[2]][1], type="variogram", name=paste0(name, "-nug"))
  WriteCharacteristic(variogram$var_model[[2]][2], type="variogram", name=paste0(name, "-psill"))
  WriteCharacteristic(variogram$var_model[[3]][2], type="variogram", name=paste0(name, "-range"))
  
  prediction <- PredictWithKriging(data, x=ConvertYearsToNum(year), observations=kObservationNum, variogram_model=variogram$var_model)
  residual <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, prediction, name)
  mse <- MSE(residual)
  
  prediction.compare <- data.frame("Год"=src.data$year[(kObservationNum + 1):src.nrows],
    "Наблюдение"=src.data$temperature[(kObservationNum + 1):src.nrows],
    "Прогноз"=prediction$var1.pred+research.data.trend[(kObservationNum + 1):src.nrows],
    "Тренд"=research.data.trend[(kObservationNum + 1):src.nrows])
  print(xtable(prediction.compare, caption=caption, label=paste0("table:", name, "-prediction"), digits=c(0, 0, 3, 3, 3)),
    file=paste0("out/variogram/", name, "-prediction.tex"))
  
  WriteCharacteristic(mse, type="variogram", name=paste0(name, "-mse"))
  
  list(variogram=variogram, prediction=prediction, residual=residual, mse=mse)
}

kObservationNum <- 32

## Form the data for research again
research.data <- src.data[0:kObservationNum, ]

research.data.fit <- lm(research.data$temperature ~ ConvertYearsToNum(research.data$year))
research.data.residuals <- research.data.fit$residuals
research.data.trend <- computeTrend(research.data.fit)

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value

# Draw H-Scatterplot
research.data.hscat <- DrawHScatterplot(research.data.residuals[1:kObservationNum], cutoff)

# Compute prediction manually with choosed model ("best" what i found)
manual <- processPrediction(data=research.data.residuals, year=research.data$year, variog=ComputeManualVariogram, cressie=FALSE, cutoff=cutoff, name="manual", caption="Прогноз (сферическая модель)")

# Compute prediction with auto fit model using classical estimation
classical <- processPrediction(data=research.data.residuals, year=research.data$year, cressie=FALSE, cutoff=cutoff, name="classical", caption="Прогноз (классическая оценка)")

# Compute prediction with auto fit model using robust (cressie) estimation
robust <- processPrediction(data=research.data.residuals, year=research.data$year, cressie=TRUE, cutoff=cutoff, name="robust", caption="Прогноз (робастная оценка)")

models.comparison <- CompareClassicalModels(manual$variogram, classical$variogram, filename="figures/variogram/models-comparison.png")

# Find best cutoff parameters
cutoff <- ComparePredictionParameters(research.data.residuals, research.data.trend, ConvertYearsToNum(research.data$year), filename="figures/variogram/parameter-comparison.png")

manual.best    <- processPrediction(data=research.data.residuals, year=research.data$year, variog=ComputeManualVariogram, cressie=FALSE, cutoff=cutoff$manual, name="manual-best", caption="Наилучший прогноз (сферическая модель)")
classcial.best <- processPrediction(data=research.data.residuals, year=research.data$year, cressie=FALSE, cutoff=cutoff$classical, name="classical-best", caption="Наилучший прогноз (классическая оценка)")
robust.best    <- processPrediction(data=research.data.residuals, year=research.data$year, cressie=TRUE, cutoff=cutoff$robust, name="robust-best", caption="Наилучший прогноз (робастная оценка)")