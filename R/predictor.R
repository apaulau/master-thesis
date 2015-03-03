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

kObservationNum <- 32

## Form the data for research again
research.data <- src.data[0:kObservationNum, ]

research.data.fit <- lm(research.data$temperature ~ ConvertYearsToNum(research.data$year))
research.data.residuals <- research.data.fit$residuals
research.data.trend <- computeTrend(research.data.fit)

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value
#cutoff <- 2

# Draw H-Scatterplot
research.data.hscat <- DrawHScatterplot(research.data.residuals[1:kObservationNum], 22)

# Compute variogram manually with choosed model (best what i could found)
variogram.manual <- ComputeManualVariogram(research.data.residuals, cutoff=cutoff, file=TRUE, file_modeled="figures/variogram/manual-model.png")

# Compute variogram with auto fit model using classical estimation
variogram.classical <- ComputeVariogram(data=research.data.residuals, x=ConvertYearsToNum(research.data$year), cressie=FALSE, cutoff=cutoff, width=FALSE,
                                        file_empirical="figures/variogram/classical-empirical.png",
                                        file_modeled="figures/variogram/classical-modeled.png")

# Compute variogram with auto fit model using robust (cressie) estimation
variogram.robust <- ComputeVariogram(data=src.data.residuals, x=ConvertYearsToNum(research.data$year), cressie=TRUE, cutoff=cutoff, width=FALSE,
                                     file_empirical="figures/variogram/robust-empirical.png",
                                     file_modeled="figures/variogram/robust-modeled.png")

models.comparison <- CompareClassicalModels(variogram.manual, variogram.classical, filename="figures/variogram/models-comparison.png")

kriging.manual    <- PredictWithKriging(research.data.residuals, x=ConvertYearsToNum(research.data$year), variogram_model=variogram.manual$var_model)
kriging.classical <- PredictWithKriging(research.data.residuals, x=ConvertYearsToNum(research.data$year), variogram_model=variogram.classical$var_model)
kriging.robust    <- PredictWithKriging(research.data.residuals, x=ConvertYearsToNum(research.data$year), variogram_model=variogram.robust$var_model)

mse.manual    <- MSE(CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.manual))
mse.classical <- MSE(CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.classical))
mse.robust    <- MSE(CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.robust))

res.ma <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.manual,    "figures/variogram/cross-prediction-manual.png")
res.cl <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.classical, "figures/variogram/cross-prediction-classical.png")
res.ro <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.robust,    "figures/variogram/cross-prediction-robust.png")

# Find best cutoff parameter
ComparePredictionParameters(research.data.residuals, research.data.trend, ConvertYearsToNum(research.data$year), filename="figures/variogram/parameter-comparison.png")


# Best prediction as we investigated is for robust kriging with cutoff=6. Let's make it!
variogram.robust.best <- ComputeVariogram(data=research.data.residuals, x=ConvertYearsToNum(research.data$year), cressie=TRUE, cutoff=6, width=FALSE,
                                          file_empirical="figures/variogram/robust-best-empirical.png",
                                          file_modeled="figures/variogram/robust-best-modeled.png")

kriging.robust.best <- PredictWithKriging(research.data.residuals, x=ConvertYearsToNum(research.data$year), variogram_model=variogram.robust.best$var_model)
mse.robust.best <- MSE(CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.robust.best))
res.ro.best <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.robust.best, "figures/variogram/cross-prediction-robust-best.png")

## TODO: form krige matrix for analysis