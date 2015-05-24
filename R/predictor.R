source("R/lib/afv.R")
source("R/lib/variogram.R")
source("R/lib/kriging.R")

## Function definition: need to be moved into isolated place
# Completes trend values up to source observation number
computeTrend <- function (fit, future=0) {
  c(sapply(c(1 : (nrows + future)), FUN=function(x) fit$coefficients[[1]] + x * fit$coefficients[[2]]))
}

# Computes prediction with passed parameters and saves all needed info and plots
processPrediction <- function (data, year, variogram, cressie, cutoff, name, caption) {
  
  prediction <- PredictWithKriging(data, x=ConvertYearsToNum(year), observations=kObservationNum, variogram_model=variogram$var_model, nrows=nrows)
  CrossPrediction(src$temperature, src$year, trend, prediction, name, observations=kObservationNum, nrows=nrows)
  residual <- ComputeKrigingResiduals(src$temperature, trend, prediction, observations=kObservationNum, nrows=nrows)
  mse <- MSE(residual)
  
  prediction.compare <- data.frame("Год"=src$year[(kObservationNum + 1):nrows],
    "Наблюдение"=src$temperature[(kObservationNum + 1):nrows],
    "Прогноз"=prediction$var1.pred+trend[(kObservationNum + 1):nrows],
    "Тренд"=trend[(kObservationNum + 1):nrows],
    "Ошибка"=residual)
  print(xtable(prediction.compare, caption=caption, label=paste0("table:", name, "-prediction"), digits=c(0, 0, 3, 3, 3, 3)),
    file=paste0("out/variogram/", name, "-prediction.tex"))
  
  WriteCharacteristic(mse, type="variogram", name=paste0(name, "-mse"))
  
  list(variogram=variogram, prediction=prediction, residual=residual, mse=mse)
}

trend <- computeTrend(sample.fit)
sample.residuals <- sample.fit$residuals

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value

# Draw H-Scatterplot
sample.hscat <- DrawHScatterplot(sample.residuals[1:kObservationNum])

lin.var1 <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin", psill=4, range=0, nugget=0, fit=FALSE)
lin.fit <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit", psill=4, range=0, nugget=0, fit=TRUE)
lin.fit.cv <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit-cv", psill=4, range=4, nugget=0, fit=FALSE)
lin.fit.adapt <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit-adapt", psill=4, range=2, nugget=0, fit=FALSE)
lin.fit.cv.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin.fit.cv, cutoff=cutoff, name="lin-fit-cv", caption="Прогноз (линейная модель с порогом)")
lin.fit.adapt.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin.fit.adapt, cutoff=cutoff, name="lin-fit-adapt", caption="Адаптивный прогноз (линейная модель с порогом)")
sph.fit.adapt <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Sph", name="sph-fit-adapt", psill=4, range=6.9, nugget=0.9, fit=FALSE)
sph.fit.adapt.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=sph.fit.adapt, cutoff=cutoff, name="sph-fit-adapt", caption="Адаптивный прогноз (сферическая модель)")
per.fit.cv <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Per", name="per-fit-cv", psill=4.1, range=0.898, nugget=0.001, fit=FALSE)
per.fit.cv.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=per.fit.cv, cutoff=cutoff, name="per-fit-cv", caption="Прогноз (периодическая модель)")

for.robust.only <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=TRUE, cutoff=20, model="Lin", name="robust", psill=0, range=0, nugget=0, fit=FALSE)

auto.class <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-20", cressie=FALSE, cutoff=20)
auto.class.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class, cutoff=20, name="auto-class-20", caption="Прогноз (волновая модель)")
auto.rob <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-rob-20", cressie=TRUE, cutoff=20)
auto.rob.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.rob, cutoff=20, name="auto-rob-20", caption="Прогноз (волновая модель)")
auto.class <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-26", cressie=FALSE, cutoff=26)
auto.class.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class, cutoff=26, name="auto-class-26", caption="Прогноз (периодическая модель)")
# Compute prediction manually with choosed model ("best" what i found)
manual <- processPrediction(data=sample.residuals, year=sample$year, variog=ComputeManualVariogram, cressie=FALSE, cutoff=cutoff, name="manual", caption="Прогноз (сферическая модель)")

cv.cutoff <- ComparePredictionParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/auto-corr-cutoff.png", observations=kObservationNum, nrows=nrows, adapt=FALSE)
adapt.cutoff <- ComparePredictionParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/auto-mse-cutoff.png", observations=kObservationNum, nrows=nrows)

auto.rob.adapt <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-rob-5", cressie=TRUE, cutoff=5)
auto.rob.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.rob.adapt, cutoff=5, name="auto-rob-5", caption="Прогноз (волновая модель)")
auto.class.adapt <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-18", cressie=FALSE, cutoff=18)
auto.class.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class.adapt, cutoff=18, name="auto-class-18", caption="Прогноз (периодическая модель)")


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