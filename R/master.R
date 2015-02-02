# Let's start from beginning.
# This file will be the master file of all diploma project's files (slaves). 
# Content will be the same as for previous works (batorino analysis).
# Some thoughts for this investigation see in TODO.Rmd.
# Ideas for organizing further research see in ideas.Rmd

## First of all we need to clean up the workspace
rm(list=ls(all=TRUE))

## Dependencies
library(ggplot2)  # eye-candy graphs
library(xtable)   # convert data to latex tables
library(outliers) # tests for outliers
library(tseries)  # adf test used
library(aplpack)  # there is bagplot functionality // WANT TO REMOVE but can't without lost bagplot graph
library(nortest)  # tests for normality
library(sp)       # spatial data
library(gstat)    # geostatistics
library(reshape2) # will see
library(spatial)

## Import local modules
source("R/lib/plotting-fun.R") # useful functions for more comfortable plotting
source("R/lib/print-fun.R")    # functions for print some data to files
source("R/lib/dstats.R")       # descriptive statistics module

# Converts years to numerical continuous representation <2010, 2011, 2012> -> <1, 2, 3>
convertYearsToNum <- function(years) {
  c(1 : (max(years) - min(years)))
}

# Returns years for which will be prediction calculated
getPredictionYears <- function (years, number, future) {
  lastYear <- max(years)
  c((lastYear - number + kObservationNum - 1) : (lastYear + future))
}

#[ Initialize block

## Read the data / pattern: year;temperature
path.data <- "data/batorino_july.csv" # this for future shiny support and may be choosing multiple data sources
src.nrows <- 38
src.data  <- read.csv(file=path.data, header=TRUE, sep=";", nrows=src.nrows, colClasses=c("numeric", "numeric"), stringsAsFactors=FALSE)

print(xtable(src.data, caption="Исходные данные.", label="table:source"),  table.placement="H", 
      file="out/source.tex")

## Global use constants
kDateBreaks <- seq(min(src.data$year) - 5, max(src.data$year) + 5, by=2) # date points for graphs

## For the reason of prediction estimation and comparison, let cut observations number by 3
kObservationNum <- length(src.data[, 1]) - 3

## Form the data for research
research.data <- src.data[0:kObservationNum, ]

#] End of initialize block
#[ Computation and analysis block

## Fitting linear model for researching data. It also compute residuals based on subtracted regression
research.data.fit <- lm(research.data$temperature ~ c(1:kObservationNum))

# Getting descriptive statistics for temperature in russian locale
research.data.dstats <- dstats.describe(research.data$temperature, locale=TRUE)
print(xtable(research.data.dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
      file="out/data_dstats.tex")

##[ Tests for normality

### Shapiro-Wilk
research.data.shapiro <- shapiro.test(research.data$temperature)
to.file(research.data.shapiro, "out/data_shapiro.tex")

### Pearson chi-square
research.data.pearson <- pearson.test(research.data$temperature)
to.file(research.data.pearson, "out/data_pearson.tex")

### Kolmogorov-Smirnov
test.nsample <- rnorm(10000, mean=mean(research.data$temperature), sd=sd(research.data$temperature)) # sample for test against source 
research.data.ks <- ks.test(x=research.data$temperature, y=test.nsample, exact=NULL)
to.file(research.data.ks, "out/data_ks.tex")

##] End tests for normality

## Grubbs test for outliers
research.data.grubbs <- grubbs.test(research.data$temperature)
to.file(research.data.grubbs, "out/data_grubbs.tex")

## Correlation matrix
research.data.cmatrix <- cor(cbind("Temperature"=research.data$temperature, "Date"=1:kObservationNum), method="pearson")
print(xtable(research.data.cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
      file="out/data_cmatrix.tex")
                             
## Pearson's product-moment correlation test. Use time for y as numerical
research.data.ctest <- cor.test(research.data$temperature, c(1:kObservationNum), method="pearson")
to.file(research.data.ctest, "out/ctest.tex")

## Next step is research residuals computed few lines above
research.residuals <- data.frame("year"=research.data$year, "temperature"=research.data.fit$residuals)
print(xtable(research.residuals, caption="Временной ряд остатков.", label="table:residuals"), table.placement="H", 
      file="out/residuals.tex")

## Descriptive statistics for residuals
research.residuals.dstats <- dstats.describe(research.residuals$temperature, locale=TRUE)
print(xtable(research.residuals.dstats, caption="Описательные статистики для остатков.", label="table:residuals_dstats"),
      file="out/residuals_dstats.tex")

##[ Tests for normality // combine with previous normality tests

### Shapiro-Wilk
research.residuals.shapiro <- shapiro.test(research.residuals$temperature)
to.file(research.residuals.shapiro, "out/residuals_shapiro.tex")

### Pearson chi-square
research.residuals.pearson <- pearson.test(research.residuals$temperature)
to.file(research.residuals.pearson, "out/residuals_pearson.tex")

### Kolmogorov-Smirnov
test.nsample <- rnorm(10000, mean=mean(research.residuals$temperature), sd=sd(research.residuals$temperature)) # sample for test against source 
research.residuals.ks <- ks.test(x=research.residuals$temperature, y=test.nsample, exact=NULL)
to.file(research.residuals.ks, "out/residuals_ks.tex")

##] End tests for normality

## Box-Ljung and adf tests (some kind of stationarity and independence tests) // TODO: need to know exactly in theory what it is
research.residuals.box <- Box.test(research.residuals$temperature, type="Ljung-Box")
to.file(research.residuals.box, "out/residuals_ljung.tex")
research.residuals.adf <- adf.test(research.residuals$temperature)
to.file(research.residuals.adf, "out/residuals_stationarity.tex")

#] End computation block
#[ Plots computation block

## Source data as basic time series plot: points connected with line
plot.source <- ggplot(src.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.source, filename="01_src.png")

## Basic histogram based on Sturges rule (by default) with pretty output (also by default)
plot.data.hist <- ggplot(research.data, aes(x=temperature), geom='blank') +   
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", 
                 binwidth=diff(range(src.data$temperature)) / nclass.Sturges(src.data$temperature), alpha=.6) +
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
plot.save(plot.data.hist, filename="02_hist.png")

## The same as previous histogram but with fitted normal distribution density curve
plot.data.hist.norm <- plot.data.hist +
  stat_function(fun=dnorm, colour='red', geom='line', 
                arg=list(mean=mean(src.data$temperature), sd=sd(src.data$temperature)))
plot.save(plot.data.hist.norm, filename="03_hist-dnorm.png")

## Normal Quantile-Quantile plot
plot.data.qq <- ggqqp(research.data$temperature)
plot.save(plot.data.qq, filename="04_qq.png")

## Bagplot. // TODO: Investigate how to replace this with new style: ggplot
#plot.data.bag <- figure.bagplot(data.frame("Date"=research.data$year, "Temperature"=research.data$temperature), title="", xlab="Год", ylab="Температура")
#to.pdf(plot.data.bag, "figures/05_bagplot.pdf", width=4, height=3)

## Scatter plot with regression line based on some magic; investigate again what is this magic is // TODO: look up can I introduce new variable with y-breaks
plot.data.scatter <- ggplot(research.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_abline(intercept=-194.632277, slope=.107706, color="blue") +
  scale_x_continuous(limits=c(min(research.data$year) - 5, max(research.data$year) + 5), breaks=kDateBreaks) + 
  scale_y_continuous(breaks=seq(10, 30, 1), limits=c(14, 26)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.data.scatter, filename="06_scatterplot.png")

## Time series (which is by default is research data) with trend line based on linear module estimate (lm)
plot.data.ts <- ggplot(research.data, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() + stat_smooth(method=lm, se=FALSE) + 
  scale_x_continuous(breaks=kDateBreaks) + scale_y_continuous(breaks=seq(16, 28, 1)) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.data.ts, filename="07_ts.png")

## Residuals time series (data have gotten on computing step: fitting linear model)
plot.residuals.ts <- ggplot(research.residuals, aes(x=year, y=temperature)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks=kDateBreaks) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
plot.save(plot.residuals.ts, filename="08_residuals.png")

## Basic histogram for residuals / seems like the same as for non-residuals
plot.residuals.hist <- ggplot(research.residuals, aes(x=temperature), geom='blank') +   
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white", binwidth=1.2, alpha=.6) +
  stat_function(fun=dnorm, colour='red', arg=list(mean=mean(research.residuals$temperature), sd=sd(research.residuals$temperature))) +    
  labs(color="") + xlab("Температура, ºС") + ylab("Плотность")
plot.save(plot.residuals.hist, filename="09_res-hist-dnorm.png")

## Normal Quantile-Quantile plot for residuals
plot.residuals.qq <- ggqqp(research.residuals$temperature)
plot.save(plot.residuals.qq, filename="10_res-qq.png")

## Auto Correlation Function plot // TODO: check the style

plot.residuals.acf <- ggacf(research.data$temperature)
plot.save(plot.residuals.acf, filename="11_acf.png")

#] End plots block

#[ Variogram analysis block

## Function definition: need to be moved into isolated place -- to be a slave

### Just definition of mean standard error // TODO: find out exact formula and describe each parameter
MSE <- function (e, N=1) {
  sum(sapply(X=e, FUN=function(x) x**2)) / N
}

### Missed complete understanding of this functionality, because it aren't used in further work. Seems like it used only for selection best parameters.
### Compares two predictions classical and robust in case of iterating through 'cutoff' param based on MSE estimation.
#### todo: simpify this function, split it to several less complex functions
comparePredictionParameters <- function (data, residuals, temperature, trend, x, y=rep(1, kObservationNum), width=1) { ### todo: usage of argument in argument default
  lens <- 1:kObservationNum
  manualResult <- c()
  classicalResult <- c()
  robustResult <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  i <- 1
  for(l in lens) {
    variogram.manual = manualVariogram(data, cutoff=l)
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=F, width=width)
    variogram.robust = autofitVariogram(data~1, spdata, cutoff=l, cressie=T, width=width)
    
    kriging.manual <- predictKrige(residuals, x=x, variogram_model=variogram.manual$var_model)
    kriging.classical <- predictKrige(residuals, x=x, variogram_model=variogram.classical$var_model)
    kriging.robust <- predictKrige(residuals, x=x, variogram_model=variogram.robust$var_model)
    
    res.manual <- crossPrediction(temperature, trend, kriging.manual)
    res.classical <- crossPrediction(temperature, trend, kriging.classical)
    res.robust <- crossPrediction(temperature, trend, kriging.robust)
    
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
compareVariogramParameters <- function (data, x, y=rep(1, kObservationNum), width) {
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

manualVariogram <- function (data, cutoff, file=FALSE, file_modeled="") {
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
calcVariogram <- function (data, x, y=rep(1, kObservationNum), file_empirical="", file_modeled="", cressie, cutoff, width) {
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
predictKrige <- function (data, x, y=rep(1, kObservationNum), variogram_model, future=0) {
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data)=~x+y
  
  new_data <- data.frame("X"=c((kObservationNum + 1):(src.nrows + future)), "Y"=rep(1, src.nrows - kObservationNum))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model)
}

## Compares predictions based on trend and kriging with actual values
crossPrediction <- function (temperature, trend, kriging, file_prediction="", future=0) {
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
                                 "year"=getPredictionYear(src.years, src.nrows, future))

  prediction.kriging <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging$var1.pred),
                                   "year"=getPredictionYear(src.years, src.nrows, future))

  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
                       "year"=getPredictionYear(src.years, src.nrows, 0))
  
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

convertYears(src.data$years) <- c(1:src.nrows)
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

variogram.manual <-  manualVariogram(src.res[1:kObservationNum], cutoff=cutoff, file=TRUE, file_modeled="figures/14_manual-mod.png")

variogram.classical <- calcVariogram(data=src.data.residuals[1:kObservationNum], x=convertYears(research.data$years), cressie=FALSE, cutoff=cutoff, width=FALSE,
                                     file_empirical="figures/13_classical-emp.png",
                                     file_modeled="figures/15_classical-mod.png")

variogram.robust <- calcVariogram(data=src.data.residuals[1:kObservationNum], x=convertYears(research.data$years), cressie=TRUE, cutoff=cutoff, width=FALSE,
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

kriging.manual <- predictKrige(src.data.residuals[1:kObservationNum], x=convertYears(research.data$years), variogram_model=variogram.manual$var_model)
kriging.classical <- predictKrige(src.data.residuals[1:kObservationNum], x=convertYears(research.data$years), variogram_model=variogram.classical$var_model)
kriging.robust <- predictKrige(src.data.residuals[1:kObservationNum], x=convertYears(research.data$years), variogram_model=variogram.robust$var_model)

mse.manual <- MSE(crossPrediction(src$temperature, src.trend, kriging.manual))
mse.classical <- MSE(crossPrediction(src$temperature, src.trend, kriging.classical))
mse.robust <- MSE(crossPrediction(src$temperature, src.trend, kriging.robust))

res.ma <- crossPrediction(src$temperature, src.data.trend, kriging.manual, "figures/cross_prediction_manual.png")
res.cl <- crossPrediction(src$temperature, src.data.trend, kriging.classical, "figures/16_cross_prediction_classical.png")
res.ro <- crossPrediction(src$temperature, src.data.trend, kriging.robust, "figures/19_cross_prediction_robust.png")

# Best prediction as we investigated is for robust kriging with cutoff=6. Let's make it!
variogram.robust.best <- calcVariogram(data=src.res[1:kObservationNum], x=src$year[1:kObservationNum], cressie=TRUE, cutoff=6, width=FALSE,
                                       file=TRUE,
                                       file_empirical="figures/20_robust-best-emp.png",
                                       file_modeled="figures/21_robust-best-mod.png")
kriging.robust.best <- predictKrige(src.res[1:kObservationNum], x=src$year[1:kObservationNum], variogram_model=variogram.robust.best$var_model, future=3)
mse.robust.best <- MSE(crossPrediction(src$temperature, src.trend, kriging.robust.best))
res.ro.best <- crossPrediction(src$temperature, src.trend, kriging.robust.best, file=TRUE, "figures/22_cross-prediction-robust-best.png")

## TODO: form krige matrix for analysis

#] End of variogram analysis block