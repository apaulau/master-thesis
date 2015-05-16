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

ComputeManualVariogram <- function (data, x, cressie=FALSE, cutoff, model="Sph", psill=0, range=3.9, nugget=3.4, fit=TRUE, name="", observations) {
  spdata <- MakeFakeSpatialData(x=x, data=data, observations=observations)
  experimentalVariogram <- variogram(data~1, spdata, width=1, cutoff=cutoff)
  
  if (psill == 0) {
    modeledVariogram <- vgm(model=model, range=range, nugget=nugget)  
  } else {
    modeledVariogram <- vgm(model=model, psill=psill, range=range, nugget=nugget)  
  }
  
  if (fit) {
    modeledVariogram <- fit.variogram(experimentalVariogram, modeledVariogram, debug.level=0)
  }
  
  if (nchar(name)) {
    SaveVariogramPlot(experimentalVariogram, modeledVariogram, name)
    print(xtable(data.frame("Модель"=modeledVariogram$model, "Порог"=modeledVariogram$psill, "Ранг"=modeledVariogram$range), 
      caption="Модель вариограммы", label="table:manual_model"), table.placement="H", file="out/variogram/manual-model.tex")
  }
  
  
  result = list(exp_var = experimentalVariogram, var_model = modeledVariogram)
  return(result)
}

## Calculates modeled variogram and creates plot of it.
ComputeVariogram <- function (data, x, cressie, cutoff, name="", observations) {
  spdata <- MakeFakeSpatialData(x=x, data=data, observations=observations)
  
  variogram <- autofitVariogram(data~1, spdata, cutoff=cutoff, cressie=cressie)
  if (nchar(name)) {
    SaveVariogramPlot(variogram$exp_var, variogram$var_model, name)
  }

  return(variogram)
}

SaveVariogramPlot <- function (experimentalVariogram, modeledVariogram, name) {
  # Arrange the data for the ggplot2 plot
  # add the semivariance values of v2 to v1
  Fitted <- data.frame(dist = seq(0.01, max(experimentalVariogram$dist), length = kObservationNum))
  Fitted$gamma <- variogramLine(modeledVariogram, dist_vector = Fitted$dist)$gamma
  #convert the dataframes to a long format
  Empirical <- melt(experimentalVariogram, id.vars = "dist", measure.vars = c("gamma"))
  Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("gamma"))
  
  filename <- paste0("figures/variogram/", name, "-variogram.png")
  
  plot.modeled <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
    geom_line(data = Modeled, color='blue') +
    scale_y_continuous(expand=c(0,0), 
      breaks=seq(0, 1.04 * max(experimentalVariogram$gamma), 1),
      limits=c(min(0, 1.04 * min(experimentalVariogram$gamma)), 1.04 * max(experimentalVariogram$gamma))) +
    scale_x_continuous(expand=c(0,0),
      breaks=seq(0, 1.04 * max(experimentalVariogram$dist), 1),
      limits=c(0, 1.04 * max(experimentalVariogram$dist))) +
    xlab("Расстояние") + ylab("Значение")
  ggsave(plot=plot.modeled, file=filename, width=7, height=4)
}

MakeFakeSpatialData <- function (x, data, observations) {
  spdata <- data.frame(cbind("x"=x, "y"=rep(1, observations), data))
  coordinates(spdata) = ~x+y
  return(spdata)
}