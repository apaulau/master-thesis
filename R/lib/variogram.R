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

  variogram
}