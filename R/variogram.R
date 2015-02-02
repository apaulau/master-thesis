## Function definition: need to be moved into isolated place -- to be a slave

### Just definition of mean standard error // TODO: find out exact formula and describe each parameter
MSE <- function (e, N=1) {
  sum(sapply(X=e, FUN=function(x) x**2)) / N
}

### Missed complete understanding of this functionality, because it aren't used in further work. Seems like it used only for selection best parameters.
### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
#### todo: simpify this function, split it to several less complex functions
ComparePredictionParameters <- function (data, residuals, temperature, trend, x, y=rep(1, kObservationNum), width=1) { ### todo: usage of argument in argument default
  lens <- 1:kObservationNum
  manualResult <- c()
  classicalResult <- c()
  robustResult <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  i <- 1
  for(l in lens) {
    variogram.manual = manualVariogram(data, cutoff=l)
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=FALSE, width=width)
    variogram.robust = autofitVariogram(data~1, spdata, cutoff=l, cressie=TRUE, width=width)
    
    kriging.manual <- PredictWithKriging(residuals, x=x, variogram_model=variogram.manual$var_model)
    kriging.classical <- PredictWithKriging(residuals, x=x, variogram_model=variogram.classical$var_model)
    kriging.robust <- PredictWithKriging(residuals, x=x, variogram_model=variogram.robust$var_model)
    
    res.manual <- CrossPrediction(temperature, trend, kriging.manual)
    res.classical <- CrossPrediction(temperature, trend, kriging.classical)
    res.robust <- CrossPrediction(temperature, trend, kriging.robust)
    
    manualResult[i] <- MSE(e=res.manual)
    classicalResult[i] <- MSE(e=res.classical)
    robustResult[i] <- MSE(e=res.robust)
    i = i + 1 ## todo: find out how to avoid this construction
  }
  
  plot.check <- ggplot() + 
    geom_line(data=data.frame("X"=lens, "Y"=manualResult), aes(x=X,y=Y)) + 
    geom_line(data=data.frame("X"=lens, "Y"=classicalResult), aes(x=X,y=Y), linetype="dashed") + 
    geom_line(data=data.frame("X"=lens, "Y"=robustResult), aes(x=X,y=Y), linetype="dotdash") + 
    scale_x_continuous(breaks=lens) +
    scale_y_continuous(breaks=seq(min(manualResult, classicalResult, robustResult), max(manualResult, classicalResult, robustResult), .3))
  ggsave(plot=plot.check, file="figures/check-dep.png", width=7, height=4)
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
  p <- data.frame("X"=c(1:kObservationNum), "Y"=rep(1,kObservationNum))
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
        
        file="out/manual_model.tex")
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
  coordinates(src_data)=~x+y
  
  new_data <- data.frame("X"=c((kObservationNum + 1):(src.nrows + future)), "Y"=rep(1, src.nrows - kObservationNum))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model)
}

## Compares predictions based on trend and kriging with actual values
CrossPrediction <- function (temperature, trend, kriging, file_prediction="", future=0) {
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
                                 "year"=GetPredictionYears(src.years, src.nrows, future))
  
  prediction.kriging <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging$var1.pred),
                                   "year"=GetPredictionYears(src.years, src.nrows, future))
  
  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
                       "year"=GetPredictionYears(src.years, src.nrows, 0))
  
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
  
  prediction.kriging$Temperature[3:(src.nrows-kObservationNum)] - actual$Temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

### once it was like this kObservationNum <- 32

### src <- read.csv(file="data/batorino_july.csv", header=TRUE, sep=";", nrows=38, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

ConvertYearsToNum(src.data$years) <- c(1:src.nrows)
src.data.fit <- lm(src$temperature ~ src$year)
src.data.residuals <- src.data.fit$residuals
src.data.trend <- src.data.fit$fitted.values

cutoff <- trunc(2 * kObservationNum / 3) # let it be "classical" value
#cutoff <- 2

# Make fake second coordinate
p <- data.frame("X"=c(1:kObservationNum), "Y"=rep(1, kObservationNum))
# Calculate distances
p.dist<-as.matrix(dist(p[,c("X", "Y")]))
dist.breaks<-quantile(p.dist,seq(.1, .9, by=.1))
coordinates(p) <- ~ X + Y
p.breaks <- (0:cutoff) * 1
hsc <- hscat(src.res[1:kObservationNum]~1, p, breaks=0:20)

to.pdf(hsc,"figures/12_hscat.pdf", width=7, height=6)

variogram.manual <-  ComputeManualVariogram(src.res[1:kObservationNum], cutoff=cutoff, file=TRUE, file_modeled="figures/14_manual-mod.png")

variogram.classical <- ComputeVariogram(data=src.data.residuals[1:kObservationNum], x=ConvertYearsToNum(research.data$years), cressie=FALSE, cutoff=cutoff, width=FALSE,
                                        file_empirical="figures/13_classical-emp.png",
                                        file_modeled="figures/15_classical-mod.png")

variogram.robust <- ComputeVariogram(data=src.data.residuals[1:kObservationNum], x=ConvertYearsToNum(research.data$years), cressie=TRUE, cutoff=cutoff, width=FALSE,
                                     file_empirical="figures/17_robust-emp.png",
                                     file_modeled="figures/18_robust-mod.png")

# Arrange the data for the ggplot2 plot
# add the semivariance values of v2 to v1
Fitted1 <- data.frame(dist = seq(.01, max(variogram.manual$exp_var$dist), length = kObservationNum))
Fitted1$gamma <- variogramLine(variogram.manual$var_model, dist_vector = Fitted1$dist)$gamma
#convert the dataframes to a long format
Empirical1 <- melt(variogram.manual$exp_var, id.vars = "dist", measure.vars = c("gamma"))
Modeled1 <- melt(Fitted1, id.vars = "dist", measure.vars = c("gamma"))

Fitted2 <- data.frame(dist = seq(.01, max(variogram.classical$exp_var$dist), length = kObservationNum))
Fitted2$gamma <- variogramLine(variogram.classical$var_model, dist_vector = Fitted2$dist)$gamma
#convert the dataframes to a long format
Empirical2 <- melt(variogram.classical$exp_var, id.vars = "dist", measure.vars = c("gamma"))
Modeled2 <- melt(Fitted2, id.vars = "dist", measure.vars = c("gamma"))

plot.modeled <- ggplot(Empirical1, aes(x = dist, y = value)) +  geom_point() + 
  geom_line(data = Modeled1, linetype="dashed") +
  geom_line(data = Modeled2) +
  labs(color="") +
  scale_y_continuous(expand=c(0,0), 
                     breaks=seq(0, 1.04 * max(variogram.manual$exp_var$gamma), 1),
                     limits=c(min(0, 1.04 * min(variogram.manual$exp_var$gamma)), 1.04 * max(variogram.manual$exp_var$gamma))) +
  scale_x_continuous(expand=c(0,0),
                     breaks=seq(0, 1.04 * max(variogram.manual$exp_var$dist), 1),
                     limits=c(0, 1.04 * max(variogram.manual$exp_var$dist))) +
  xlab("Расстояние") + ylab("Значение")
ggsave(plot=plot.modeled, file="figures/14_var_models.png", width=7, height=4)

kriging.manual <- PredictWithKriging(src.data.residuals[1:kObservationNum], x=ConvertYearsToNum(research.data$years), variogram_model=variogram.manual$var_model)
kriging.classical <- PredictWithKriging(src.data.residuals[1:kObservationNum], x=ConvertYearsToNum(research.data$years), variogram_model=variogram.classical$var_model)
kriging.robust <- PredictWithKriging(src.data.residuals[1:kObservationNum], x=ConvertYearsToNum(research.data$years), variogram_model=variogram.robust$var_model)

mse.manual <- MSE(CrossPrediction(src$temperature, src.trend, kriging.manual))
mse.classical <- MSE(CrossPrediction(src$temperature, src.trend, kriging.classical))
mse.robust <- MSE(CrossPrediction(src$temperature, src.trend, kriging.robust))

res.ma <- CrossPrediction(src$temperature, src.data.trend, kriging.manual, "figures/cross_prediction_manual.png")
res.cl <- CrossPrediction(src$temperature, src.data.trend, kriging.classical, "figures/16_cross_prediction_classical.png")
res.ro <- CrossPrediction(src$temperature, src.data.trend, kriging.robust, "figures/19_cross_prediction_robust.png")

# Best prediction as we investigated is for robust kriging with cutoff=6. Let's make it!
variogram.robust.best <- ComputeVariogram(data=src.res[1:kObservationNum], x=src$year[1:kObservationNum], cressie=TRUE, cutoff=6, width=FALSE,
                                          file=TRUE,
                                          file_empirical="figures/20_robust-best-emp.png",
                                          file_modeled="figures/21_robust-best-mod.png")
kriging.robust.best <- PredictWithKriging(src.res[1:kObservationNum], x=src$year[1:kObservationNum], variogram_model=variogram.robust.best$var_model, future=3)
mse.robust.best <- MSE(CrossPrediction(src$temperature, src.trend, kriging.robust.best))
res.ro.best <- CrossPrediction(src$temperature, src.trend, kriging.robust.best, file=TRUE, "figures/22_cross-prediction-robust-best.png")

## TODO: form krige matrix for analysis