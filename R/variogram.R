source("R/archive/variogram_analysis/afv.R")

## Function definition: need to be moved into isolated place

### Just definition of mean standard error // TODO: find out exact formula and describe each parameter
MSE <- function (e, N=1) {
  sum(sapply(X=e, FUN=function(x) x**2)) / length(e)
}

CompareClassicalModels <- function(manual, classical, filename) {
  # Arrange the data for the ggplot2 plot
  # add the semivariance values of v2 to v1
  Fitted1 <- data.frame(dist = seq(.01, max(manual$exp_var$dist), length = kObservationNum))
  Fitted1$gamma <- variogramLine(manual$var_model, dist_vector = Fitted1$dist)$gamma
  #convert the dataframes to a long format
  Empirical1 <- melt(manual$exp_var, id.vars = "dist", measure.vars = c("gamma"))
  Modeled1 <- melt(Fitted1, id.vars = "dist", measure.vars = c("gamma"))
  
  Fitted2 <- data.frame(dist = seq(.01, max(classical$exp_var$dist), length = kObservationNum))
  Fitted2$gamma <- variogramLine(classical$var_model, dist_vector = Fitted2$dist)$gamma
  #convert the dataframes to a long format
  Empirical2 <- melt(classical$exp_var, id.vars = "dist", measure.vars = c("gamma"))
  Modeled2 <- melt(Fitted2, id.vars = "dist", measure.vars = c("gamma"))
  
  plot.modeled <- ggplot(Empirical1, aes(x = dist, y = value)) +  geom_point() + 
    geom_line(data = Modeled1, linetype="dashed") +
    geom_line(data = Modeled2) +
    labs(color="") +
    scale_y_continuous(expand=c(0,0), 
      breaks=seq(0, 1.04 * max(manual$exp_var$gamma), 1),
      limits=c(min(0, 1.04 * min(manual$exp_var$gamma)), 1.04 * max(manual$exp_var$gamma))) +
    scale_x_continuous(expand=c(0,0),
      breaks=seq(0, 1.04 * max(manual$exp_var$dist), 1),
      limits=c(0, 1.04 * max(manual$exp_var$dist))) +
    xlab("Расстояние") + ylab("Значение")
  ggsave(plot=plot.modeled, file=filename, width=7, height=4)
  
  plot.modeled
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

### This comparison is worth than above one, the estimation of it's goodness is simpler // TODO: check, maybe it should be removed. 
### I don't see the difference and profit of this kind of comparison. Maybe it should be changed to more universal way (e.g. to pass estimation function).
### Update. Now I feel the difference. Above case is better but may be both have rights to live together, will see.
### Update of update. Hmm, this one compares only variogram calculations. The estimate based on sserr divided by length.
CompareVariogramParameters <- function (data, x, y=rep(1, kObservationNum), width) {
  lens <- 1:kObservationNum
  classicalResult <- c()
  robustResult <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata) = ~x+y
  
  i <- 1
  for(l in lens) {
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=FALSE, width=width)
    variogram.robust = autofitVariogram(data~1, spdata, cutoff=l, cressie=TRUE, width=width)
    classicalResult[i] <- variogram.classical$sserr / l
    robustResult[i] <- variogram.robust$sserr / l
    i = i + 1
  }
  
  ggplot() + 
    geom_line(data=data.frame("X"=lens, "Y"=classicalResult), aes(x=X, y=Y, color="classic")) + 
    geom_line(data=data.frame("X"=lens, "Y"=robustResult), aes(x=X, y=Y, color="cressie")) + 
    scale_x_continuous(breaks=lens) +
    scale_y_continuous(breaks=seq(1.04 * min(classicalResult, robustResult), 1.04 * max(classicalResult, robustResult), 1))
}

ComputeManualVariogram <- function (data, cutoff, file=FALSE, file_modeled="") {
  # Make fake second coordinate
  p <- data.frame("X"=c(1:kObservationNum), "Y"=rep(1, kObservationNum))
  coordinates(p) <- ~ X + Y
  experimental_variogram <- variogram(data~1, p, width=1, cutoff=cutoff)
  
  model.variog <- vgm(model="Sph", range=3.9, nugget=3.4)  
  fit.variog <- fit.variogram(experimental_variogram, model.variog)
  
  if (file) {
    # Arrange the data for the ggplot2 plot
    # add the semivariance values of v2 to v1
    Fitted <- data.frame(dist = seq(0.01, max(experimental_variogram$dist), length = kObservationNum))
    Fitted$gamma <- variogramLine(fit.variog, dist_vector = Fitted$dist)$gamma
    #convert the dataframes to a long format
    Empirical <- melt(experimental_variogram, id.vars = "dist", measure.vars = c("gamma"))
    Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("gamma"))
    
    plot.modeled <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
      geom_line(data = Modeled, color='blue') +
      scale_y_continuous(expand=c(0,0), 
                         breaks=seq(0, 1.04 * max(experimental_variogram$gamma), 1),
                         limits=c(min(0, 1.04 * min(experimental_variogram$gamma)), 1.04 * max(experimental_variogram$gamma))) +
      scale_x_continuous(expand=c(0,0),
                         breaks=seq(0, 1.04 * max(experimental_variogram$dist), 1),
                         limits=c(0, 1.04 * max(experimental_variogram$dist))) +
      xlab("Расстояние") + ylab("Значение")
    ggsave(plot=plot.modeled, file=file_modeled, width=7, height=4)
  }
  print(xtable(data.frame("Модель"=fit.variog$model, "Порог"=fit.variog$psill, "Ранг"=fit.variog$range), caption="Модель вариограммы", label="table:manual_model"), table.placement="H", 
        file="out/variogram/manual-model.tex")
  result = list(exp_var = experimental_variogram, var_model = fit.variog)
}

## Calculates modeled variogram and creates plot of it.
ComputeVariogram <- function (data, x, y=rep(1, kObservationNum), file_empirical="", file_modeled="", cressie, cutoff, width) {
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata) = ~x+y
  
  variogram <- autofitVariogram(data~1, spdata, cutoff=cutoff, cressie=cressie, width=width)
  if (nchar(file_empirical)) { ## here was another check: just <file>
    # Arrange the data for the ggplot2 plot
    # add the semivariance values of v2 to v1
    Fitted <- data.frame(dist = seq(.01, max(variogram$exp_var$dist), length = kObservationNum))
    Fitted$gamma <- variogramLine(variogram$var_model, dist_vector = Fitted$dist)$gamma
    #convert the dataframes to a long format
    Empirical <- melt(variogram$exp_var, id.vars="dist", measure.vars=c("gamma"))
    Modeled <- melt(Fitted, id.vars="dist", measure.vars=c("gamma"))
    
    plot.empirical <- ggplot(Empirical, aes(x=dist, y=value)) +  geom_point() + 
      scale_y_continuous(expand = c(0, 0), breaks=seq(0, 7, 1), limits=c(min(0, 1.04 * min(variogram$exp_var$gamma)), 1.04 * max(variogram$exp_var$gamma))) +
      scale_x_continuous(expand = c(0, 0), breaks=seq(0, 1.04 * max(variogram$exp_var$dist), 2), limits=c(0, 1.04 * max(variogram$exp_var$dist))) +
      xlab("Расстояние") + ylab("Значение")
    ggsave(plot=plot.empirical, file=file_empirical, width=7, height=4)
  }
  if (nchar(file_modeled)) {
    plot.modeled <- ggplot(Empirical, aes(x=dist, y=value)) +  geom_point() + 
      geom_line(data=Modeled, color='blue') +
      scale_y_continuous(expand=c(0, 0), 
                         breaks=seq(0, 1.04 * max(variogram$exp_var$gamma), 1),
                         limits=c(min(0, 1.04 * min(variogram$exp_var$gamma)), 1.04 * max(variogram$exp_var$gamma))) +
      scale_x_continuous(expand=c(0, 0),
                         breaks=seq(0, 1.04 * max(variogram$exp_var$dist), 1),
                         limits=c(0, 1.04 * max(variogram$exp_var$dist))) +
      xlab("Расстояние") + ylab("Значение")
    ggsave(plot=plot.modeled, file=file_modeled, width=7, height=4)
  }
  #   plot(variogram)
  variogram
}

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
      geom_line(data=prediction.kriging, aes(x=year, y=temperature, color="Прогноз Кригинг")) + 
      geom_line(data=prediction.trend, aes(x=year, y=temperature, color="Прогноз Тренд")) +
      geom_line(data=actual, aes(x=year, y=temperature, colour="Актуальное")) +
      labs(color="") +
      scale_x_continuous(breaks=seq(min(actual$year), max(actual$year) + 5 + future, by=1)) + xlab("Год наблюдения") +
      scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, С") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(color="")
    ggsave(plot=plot.crossprediction, file=file_prediction, width=7, height=4)
  }
  
  prediction.kriging$temperature[3:(src.nrows-kObservationNum)] - actual$temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

### once it was like this kObservationNum <- 32

### src <- read.csv(file="data/batorino_july.csv", header=TRUE, sep=";", nrows=38, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

# Completes trend values to source observation number
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

res.ma <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.manual, "figures/variogram/cross-prediction-manual.png")
res.cl <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.classical, "figures/variogram/cross-prediction-classical.png")
res.ro <- CrossPrediction(src.data$temperature, src.data$year, research.data.trend, kriging.robust, "figures/variogram/cross-prediction-robust.png")

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