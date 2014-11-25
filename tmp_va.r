library(sp)
#library(gstat)
library(ggplot2)
library(reshape2)

source("R/variogram_analysis/afv.r")
#source("R/variogram_analysis/afk.r")

MSE <- function (e, N=1) {
  sum(sapply(X=e, FUN=function(x) x**2)) / N
}

superCheckDependency <- function (data, x, y=rep(1, OBS_NUM), width=1) {
  lens <- 1:OBS_NUM
  res1 <- c()
  res2 <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  i <- 1
  for(l in lens) {
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=F, width=width)
    variogram.robust = autofitVariogram(data~1, spdata, cutoff=l, cressie=T, width=width)
    
    kriging.classical <- predictKrige(src.res[1:OBS_NUM], x=x, variogram_model=variogram.classical$var_model)
    kriging.robust <- predictKrige(src.res[1:OBS_NUM], x=x, variogram_model=variogram.robust$var_model)
    
    res.classical <- crossPrediction(src$Temperature, src.trend, kriging.classical)
    res.robust <- crossPrediction(src$Temperature, src.trend, kriging.robust)
    res1[i] <- MSE(e=res.classical)
    res2[i] <- MSE(e=res.robust)
    i = i +1
  }
  df1 <- data.frame("X"=lens, "Y"=res1)
  df2 <- data.frame("X"=lens, "Y"=res2)
  ggplot() + 
    geom_line(data=df1, aes(x=X,y=Y, color="classic")) + 
    geom_line(data=df2, aes(x=X,y=Y,color="cressie")) + 
    scale_x_continuous(breaks=lens) +
    scale_y_continuous(breaks=seq(min(res1, res2), max(res1, res2), .3))
}


checkDependency <- function (data, x, y=rep(1, OBS_NUM), width) {
  lens <- 1:OBS_NUM
  res1 <- c()
  res2 <- c()
  
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  i <- 1
  for(l in lens) {
    variogram.classical = autofitVariogram(data~1, spdata, cutoff=l, cressie=F, width=width)
    variogram.robust = autofitVariogram(data~1, spdata, cutoff=l, cressie=T, width=width)
    res1[i] <- variogram.classical$sserr/l
    res2[i] <- variogram.robust$sserr/l
    i = i + 1
  }
  df1 <- data.frame("X"=lens, "Y"=res1)
  df2 <- data.frame("X"=lens, "Y"=res2)
  ggplot() + 
    geom_line(data=df1, aes(x=X,y=Y, color="classic")) + 
    geom_line(data=df2, aes(x=X,y=Y,color="cressie")) + 
    scale_x_continuous(breaks=lens) +
    scale_y_continuous(breaks=seq(1.04*min(res1, res2), 1.04*max(res1, res2), 1))
}

calcVariogram <- function (data, x, y=rep(1, OBS_NUM), file_empirical=F, file_modeled=F, cressie, cutoff, width) {
  spdata <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(spdata)=~x+y
  
  variogram <- autofitVariogram(data~1, spdata, cutoff=cutoff, cressie=cressie, width=width)
  if (file_empirical) {
    # Arrange the data for the ggplot2 plot
    # add the semivariance values of v2 to v1
    Fitted <- data.frame(dist = seq(0.01, max(variogram$exp_var$dist), length = OBS_NUM))
    Fitted$gamma <- variogramLine(variogram$var_model, dist_vector = Fitted$dist)$gamma
    #convert the dataframes to a long format
    Empirical <- melt(variogram$exp_var, id.vars = "dist", measure.vars = c("gamma"))
    Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("gamma"))
    
    plot.empirical <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
      scale_y_continuous(expand = c(0,0),breaks=seq(0,7,1), limits=c(min(0, 1.04 * min(variogram$exp_var$gamma)), 1.04 * max(variogram$exp_var$gamma))) +
      scale_x_continuous(expand = c(0,0),breaks=seq(0,1.04 * max(variogram$exp_var$dist),2), limits=c(0,1.04 * max(variogram$exp_var$dist))) +
      xlab("Расстояние") + ylab("Значение")
    ggsave(plot=plot.empirical, file=file_empirical, width=7, height=4)
  }
  if (file_modeled) {
    plot.modeled <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
      geom_line(data = Modeled, color='blue') +
      scale_y_continuous(expand=c(0,0), 
                         breaks=seq(0, 1.04*max(variogram$exp_var$gamma), 1),
                         limits=c(min(0, 1.04*min(variogram$exp_var$gamma)), 1.04*max(variogram$exp_var$gamma))) +
      scale_x_continuous(expand=c(0,0),
                         breaks=seq(0, 1.04*max(variogram$exp_var$dist), 1),
                         limits=c(0, 1.04*max(variogram$exp_var$dist))) +
      xlab("Расстояние") + ylab("Значение")
    ggsave(plot=plot.modeled, file=file_modeled, width=7, height=4)
  }
#   plot(variogram)
  variogram
}

predictKrige <- function (data, x, y=rep(1, OBS_NUM), variogram_model) {
  src_data <- data.frame(cbind("x"=x, "y"=y, data))
  coordinates(src_data)=~x+y
  
  new_data <- data.frame("X"=c((OBS_NUM+1):38), "Y"=rep(1, 38 - OBS_NUM))
  coordinates(new_data) = ~X+Y
  
  krige(data~1, src_data, new_data, model=variogram_model)
}

crossPrediction <- function (temperature, trend, kriging, file_prediction=F) {
  prediction.trend <- data.frame("Temperature"=c(temperature[(OBS_NUM-1):OBS_NUM], trend[(OBS_NUM+1):38]),
                                 "Year"=c((2012-38+OBS_NUM-1):2012))
  prediction.kriging <- data.frame("Temperature"=c(temperature[(OBS_NUM-1):OBS_NUM], trend[(OBS_NUM+1):38]+kriging$var1.pred),
                                   "Year"=c((2012-38+OBS_NUM-1):2012))
  actual <- data.frame("Temperature"=temperature[(OBS_NUM-1):38], 
                       "Year"=c((2012-38+OBS_NUM-1):2012))
  if (file_prediction) {
    plot.crossprediction <- ggplot() +
      geom_line(data=prediction.kriging, aes(x=Year, y=Temperature, color="Прогноз Кригинг")) + 
      geom_line(data=prediction.trend, aes(x=Year, y=Temperature, color="Прогноз Тренд")) +
      geom_line(data=actual, aes(x=Year, y=Temperature, colour="Актуальное")) +
      scale_x_continuous(breaks=seq(min(actual$Year), max(actual$Year)+5, by=1)) + xlab("Год наблюдения") +
      scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, ºС") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(color="")
    ggsave(plot=plot.crossprediction, file=file_prediction, width=7, height=4)
  }
  
  prediction.kriging$Temperature[3:(38-OBS_NUM)]-actual$Temperature[3:(38-OBS_NUM)]
}

OBS_NUM <- 37

src <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

src$Year <- c(1:38)
src.fit <- lm(src$Temperature ~ src$Year)
src.res <- src.fit$residuals
src.trend <- src.fit$fitted.values

# cutoff <- trunc(2 * OBS_NUM / 3) # let it be "classical" value
cutoff <- 6
variogram.classical <- calcVariogram(data=src.res[1:OBS_NUM], x=src$Year[1:OBS_NUM], cressie=F, cutoff=cutoff, width=F,
                                     file_empirical="figures/variog_classical_emp.png",
                                     file_modeled="figures/variog_classical_mod.png")

variogram.robust <- calcVariogram(data=src.res[1:OBS_NUM], x=src$Year[1:OBS_NUM], cressie=T, cutoff=cutoff, width=F,
                                  file_empirical="figures/variog_robust_emp.png",
                                  file_modeled="figures/variog_robust_mod.png")

kriging.classical <- predictKrige(src.res[1:OBS_NUM], x=src$Year[1:OBS_NUM], variogram_model=variogram.classical$var_model)
kriging.robust <- predictKrige(src.res[1:OBS_NUM], x=src$Year[1:OBS_NUM], variogram_model=variogram.robust$var_model)

res.cl <- crossPrediction(src$Temperature, src.trend, kriging.classical, "figures/cross_prediction_classical.png")
res.ro <- crossPrediction(src$Temperature, src.trend, kriging.robust, "figures/cross_prediction_robust.png")

## TODO: form krige matrix for analysis