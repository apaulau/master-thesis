source("R/lib/afv.R")
source("R/lib/variogram.R")
source("R/lib/kriging.R")

# Completes trend values up to source observation number
computeTrend <- function (fit, future=0) {
  c(sapply(c(1 : (nrows + future)), FUN=function(x) fit$coefficients[[1]] + x * fit$coefficients[[2]]))
}

computePrediction <- function(pred, trend) {
  pred$var1.pred + trend[(kObservationNum + 1):nrows]
}

# Computes prediction with passed parameters and saves all needed info and plots
processPrediction <- function (data, year, variogram, cressie, cutoff, name, caption, place="ht") {
  
  prediction <- PredictWithKriging(data, x=ConvertYearsToNum(year), observations=kObservationNum, variogram_model=variogram$var_model, nrows=nrows)
  CrossPrediction(src$temperature, src$year, trend, prediction, name, observations=kObservationNum, nrows=nrows)
  residual <- ComputeKrigingResiduals(src$temperature, trend, prediction, observations=kObservationNum, nrows=nrows)
  mse <- MSE(residual)
  
  prediction.compare <- data.frame("Год"=src$year[(kObservationNum + 1):nrows],
    "Наблюдение"=src$temperature[(kObservationNum + 1):nrows],
    "Прогноз"=prediction$var1.pred+trend[(kObservationNum + 1):nrows],
    "Тренд"=trend[(kObservationNum + 1):nrows],
    "Ошибка"=residual)
  colnames(prediction.compare) <- c("", "$X(t)$", "$X^{*}(t)$", "$y(t)$", "$ X(t) - X^{*}(t) $")
  print(xtable(prediction.compare, caption=caption, label=paste0("table:", name, "-prediction"), digits=c(0, 0, 3, 3, 3, 3), align="rr|cccc"),
    file=paste0("out/variogram/", name, "-prediction.tex"), sanitize.text.function=function(x){x}, include.rownames=FALSE, table.placement=place, caption.placement = 'top')
  
  WriteCharacteristic(mse, type="variogram", name=paste0(name, "-mse"))
  
  list(variogram=variogram, prediction=prediction, residual=residual, mse=mse)
}

trend <- computeTrend(sample.fit)
sample.residuals <- sample.fit$residuals

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value

# Draw H-Scatterplot
sample.hscat <- DrawHScatterplot(sample.residuals[1:kObservationNum])

lin <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin", psill=4, range=0, nugget=0.00001, fit=FALSE)
lin.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin, cutoff=cutoff, name="lin", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_1(h) $)")
lin.fit <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit", psill=4, range=0, nugget=0, fit=TRUE)
lin.fit.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin.fit, cutoff=cutoff, name="lin-fit", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_2(h) $)", place="H")
lin.fit.cv <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit-cv", psill=4, range=4, nugget=0, fit=FALSE)
lin.fit.adapt <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Lin", name="lin-fit-adapt", psill=4, range=2, nugget=0, fit=FALSE)
lin.fit.cv.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin.fit.cv, cutoff=cutoff, name="lin-fit-cv", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_3(h) $)", place="H")
lin.fit.adapt.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=lin.fit.adapt, cutoff=cutoff, name="lin-fit-adapt", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_4(h) $)")
sph.fit.adapt <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Sph", name="sph-fit-adapt", psill=4, range=6.9, nugget=0.9, fit=FALSE)
sph.fit.adapt.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=sph.fit.adapt, cutoff=cutoff, name="sph-fit-adapt", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_5(h) $)")
per.fit.cv <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=FALSE, cutoff=20, model="Per", name="per-fit-cv", psill=4.1, range=0.898, nugget=0.001, fit=FALSE)
per.fit.cv.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=per.fit.cv, cutoff=cutoff, name="per-fit-cv", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_6(h) $)", place="H")

for.robust.only <- ComputeManualVariogram(data=sample.residuals, x=sample$year, cressie=TRUE, cutoff=20, model="Lin", name="robust", psill=0, range=0, nugget=0, fit=FALSE)

auto.class20 <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-20", cressie=FALSE, cutoff=20)
auto.class20.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class20, cutoff=20, name="auto-class-20", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_7(h) $)")
auto.class26 <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-26", cressie=FALSE, cutoff=26)
auto.class26.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class26, cutoff=26, name="auto-class-26", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_8(h) $)")

cv.cutoff <- ComparePredictionParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/auto-corr-cutoff.png", observations=kObservationNum, nrows=nrows, adapt=FALSE)
adapt.cutoff <- ComparePredictionParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/auto-mse-cutoff.png", observations=kObservationNum, nrows=nrows)

auto.rob.adapt <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-rob-5", cressie=TRUE, cutoff=5)
auto.rob.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.rob.adapt, cutoff=5, name="auto-rob-5", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_9(h) $)", place="H")
auto.class.adapt <- ComputeVariogram(data=sample.residuals, x=sample$year, name="auto-class-18", cressie=FALSE, cutoff=18)
auto.class.adapt.prediction <- processPrediction(data=sample.residuals, year=sample$year, variogram=auto.class.adapt, cutoff=18, name="auto-class-18", caption="Прогнозные значения (модель $ \\widehat{\\gamma}_{10}(h) $)")

comp <- data.frame("Год"=src$year[(kObservationNum + 1):nrows],
  "actual"=src$temperature[(kObservationNum + 1):nrows],
  "lin"=computePrediction(lin.prediction$prediction, trend), 
  "lin-fit"=computePrediction(lin.fit.prediction$prediction, trend), 
  "lin-fit-cv"=computePrediction(lin.fit.cv.prediction$prediction, trend), 
  "lin-fit-adapt"=computePrediction(lin.fit.adapt.prediction$prediction, trend),
  "sph-fit-adapt"=computePrediction(sph.fit.adapt.prediction$prediction, trend),
  "per-fit-cv"=computePrediction(per.fit.cv.prediction$prediction, trend),
  "auto-class-20"=computePrediction(auto.class20.prediction$prediction, trend),
  "auto-class-26"=computePrediction(auto.class26.prediction$prediction, trend),
  "auto-rob-5"=computePrediction(auto.rob.prediction$prediction, trend),
  "auto-class-18"=computePrediction(auto.class.adapt.prediction$prediction, trend))
colnames(comp) <- c("Год", "X(t)", 
  "$ X^{*}_1(t) $",
  "$ X^{*}_2(t) $",
  "$ X^{*}_3(t) $",
  "$ X^{*}_4(t) $",
  "$ X^{*}_5(t) $",
  "$ X^{*}_6(t) $",
  "$ X^{*}_7(t) $",
  "$ X^{*}_8(t) $",
  "$ X^{*}_9(t) $",
  "$ X^{*}_{10}(t) $")
print(xtable(comp, caption="Сводная таблица реальных $ X(t)$ и прогнозных $ X_i^{*}()t, i = \overline{1,10} $ значений", label="table:summary-prediction", digits=c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), align="rr|rcccccccccc"),
  file="out/variogram/summary-prediction1.tex", sanitize.text.function=function(x){x}, include.rownames=FALSE, size="footnotesize", caption.placement = 'top')

comp.st <- data.frame(
  "lin"=computePlainStatistics(computeCV(sample.residuals, lin$var_model, kObservationNum, nfold=kObservationNum)),
  "lin-fit"=computePlainStatistics(computeCV(sample.residuals, lin.fit$var_model, kObservationNum, nfold=kObservationNum)),
  "lin-fit-cv"=computePlainStatistics(computeCV(sample.residuals, lin.fit.cv$var_model, kObservationNum, nfold=kObservationNum)), 
  "lin-fit-adapt"=computePlainStatistics(computeCV(sample.residuals, lin.fit.adapt$var_model, kObservationNum, nfold=kObservationNum)),
  "sph-fit-adapt"=computePlainStatistics(computeCV(sample.residuals, sph.fit.adapt$var_model, kObservationNum, nfold=kObservationNum)),
  "per-fit-cv"=computePlainStatistics(computeCV(sample.residuals, per.fit.cv$var_model, kObservationNum, nfold=kObservationNum)),
  "auto-class-20"=computePlainStatistics(computeCV(sample.residuals, auto.class20$var_model, kObservationNum, nfold=kObservationNum)),
  "auto-class-26"=computePlainStatistics(computeCV(sample.residuals, auto.class26$var_model, kObservationNum, nfold=kObservationNum)),
  "auto-rob-5"=computePlainStatistics(computeCV(sample.residuals, auto.rob.adapt$var_model, kObservationNum, nfold=kObservationNum)),
  "auto-class-18"=computePlainStatistics(computeCV(sample.residuals, auto.class.adapt.prediction$var_model, kObservationNum, nfold=kObservationNum)))
colnames(comp.st) <- c( 
  "$ \\widehat{\\gamma}_1(h) $",
  "$ \\widehat{\\gamma}_2(h) $",
  "$ \\widehat{\\gamma}_3(h) $",
  "$ \\widehat{\\gamma}_4(h) $",
  "$ \\widehat{\\gamma}_5(h) $",
  "$ \\widehat{\\gamma}_6(h) $",
  "$ \\widehat{\\gamma}_7(h) $",
  "$ \\widehat{\\gamma}_8(h) $",
  "$ \\widehat{\\gamma}_9(h) $",
  "$ \\widehat{\\gamma}_{10}(h) $")
rownames(comp.st) <- c(
  "$ S $", "$ E $", "$ MAE $", "$ MSE $", "$ r_{\\varepsilon\\varepsilon^{*}} $")

print(xtable(comp.st, caption="Сводная таблица показателей качества моделей семивариограмм", label="table:summary-cv", digits=c(0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), align="r|cccccccccc"),
  file="out/variogram/summary-cv1.tex", sanitize.text.function=function(x){x}, size="footnotesize", )

plot.lin.range.cv <- RunThroughParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/parameter-comparison.png", observations=kObservationNum, nrows=nrows, cutoff=16, model="Lin", nugget=0.000001, sill=4, range=NA, max=10, cressie=F, adapt=F)
ggsave(plot=plot.lin.range.cv, file="figures/variogram/lin-range-corr.png", width=7, height=3.3)

plot.lin.range.adapt <- RunThroughParameters(sample.residuals, trend, ConvertYearsToNum(sample$year), filename="figures/variogram/parameter-comparison.png", observations=kObservationNum, nrows=nrows, cutoff=16, model="Lin", nugget=0.000001, sill=4, range=NA, max=10, cressie=F, adapt=T)
ggsave(plot=plot.lin.range.adapt, file="figures/variogram/lin-range-adapt.png", width=7, height=3.3)