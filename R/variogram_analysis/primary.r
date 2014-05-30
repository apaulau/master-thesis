library(gstat)
library(ggplot2)
library(reshape2)
OBS_NUM <- 30
len <- 30
len1 <- 1

data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=OBS_NUM,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

data$Date <- c(1:OBS_NUM)
data.fit <- lm(data$Temperature ~ data$Date)
data.res <- data.fit$residuals
#data.res <- data$Temperature

spdata <- data.frame(cbind("x"=data$Date, "y"=rep(1,OBS_NUM), data.res))
coordinates(spdata)=~x+y


variogram1 = autofitVariogram1(data.res~1, spdata, len=len1, verbose=T)
variogram = autofitVariogram(data.res~1, spdata, len=len, verbose=T)


# Arrange the data for the ggplot2 plot
# add the semivariance values of v2 to v1
Fitted <- data.frame(dist = seq(0.01, max(variogram$exp_var$dist), length = OBS_NUM))
Fitted$gamma <- variogramLine(variogram$var_model, dist_vector = Fitted$dist)$gamma
#convert the dataframes to a long format
Empirical <- melt(variogram$exp_var, id.vars = c("np","dist"), measure.vars = c("gamma"))
Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("gamma"))

Fitted1 <- data.frame(dist = seq(0.01, max(variogram1$exp_var$dist), length = OBS_NUM))
Fitted1$gamma <- variogramLine(variogram1$var_model, dist_vector = Fitted1$dist)$gamma
#convert the dataframes to a long format
Empirical1 <- melt(variogram1$exp_var, id.vars = c("np","dist"), measure.vars = c("gamma"))
Modeled1 <- melt(Fitted1, id.vars = "dist", measure.vars = c("gamma"))

variog1.emp <- ggplot(Empirical1, aes(x = dist, y = value)) +  geom_point() + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,7,1), limits=c(min(0, 1.04 * min(variogram1$exp_var$gamma)), 1.04 * max(variogram1$exp_var$gamma))) +
  scale_x_continuous(expand = c(0,0),breaks=seq(0,1.04 * max(variogram1$exp_var$dist),2), limits=c(0,1.04 * max(variogram1$exp_var$dist))) +
  xlab("Расстояние") + ylab("Значение")
ggsave(plot=variog1.emp, file="figures/article/variog1_emp.png", width=7, height=4)

#one plot for each variogram
#variogp <- ggplot(Empirical, aes(x = dist, y = value)) + 
variog1.mod <- ggplot(Empirical1, aes(x = dist, y = value)) +  geom_point() + 
  geom_line(data = Modeled1, color='blue') +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,7,1), limits=c(min(0, 1.04 * min(variogram1$exp_var$gamma)), 1.04 * max(variogram1$exp_var$gamma))) +
  scale_x_continuous(expand = c(0,0),breaks=seq(0,1.04 * max(variogram1$exp_var$dist),2), limits=c(0,1.04 * max(variogram1$exp_var$dist))) +
  xlab("Расстояние") + ylab("Значение")
ggsave(plot=variog1.mod, file="figures/article/variog1_mod.png", width=7, height=4)

variog2.emp <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,7,1), limits=c(min(0, 1.04 * min(variogram$exp_var$gamma)), 1.04 * max(variogram$exp_var$gamma))) +
  scale_x_continuous(expand = c(0,0),breaks=seq(0,1.04 * max(variogram$exp_var$dist),2), limits=c(0,1.04 * max(variogram$exp_var$dist))) +
  xlab("Расстояние") + ylab("Значение")
ggsave(plot=variog2.emp, file="figures/article/variog2_emp.png", width=7, height=4)

#one plot for each variogram
#variogp <- ggplot(Empirical, aes(x = dist, y = value)) + 
variog2.mod <- ggplot(Empirical, aes(x = dist, y = value)) +  geom_point() + 
  geom_line(data = Modeled, color='blue') +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,7,1), limits=c(min(0, 1.04 * min(variogram$exp_var$gamma)), 1.04 * max(variogram$exp_var$gamma))) +
  scale_x_continuous(expand = c(0,0),breaks=seq(0,1.04 * max(variogram$exp_var$dist),2), limits=c(0,1.04 * max(variogram$exp_var$dist))) +
  xlab("Расстояние") + ylab("Значение")
ggsave(plot=variog2.mod, file="figures/article/variog2_mod.png", width=7, height=4)

trend <- function(t) {
  18.0521 + 0.1014*t
}

new_data <- data.frame("X"=c((OBS_NUM+1):38), "Y"=rep(1,38 - OBS_NUM))
coordinates(new_data) = ~X+Y

len <- 15
k = autoKrige(data.res~1,spdata,new_data, len=len)
len1 <- 15
#k = autoKrige1(data.res~1,spdata,new_data, len=len1)

d <- sapply(c((OBS_NUM+1):38), FUN=trend)
## TODO: form krige matrix for analysis

predpred <- data.frame("Temperature"=c(data$Temperature[(OBS_NUM-1):OBS_NUM], d), "Year"=c((2012-38+OBS_NUM-1):2012))
pred <- data.frame("Temperature"=c(data$Temperature[(OBS_NUM-1):OBS_NUM], k$krige_output$var1.pred+d), "Year"=c((2012-38+OBS_NUM-1):2012))
#pred <- data.frame("Temperature"=c(data$Temperature[34:35], k$krige_output$var1.pred[1], k$krige_output$var1.pred[2], k$krige_output$var1.pred[3]), "Year"=c(2008:2012))
act <- data.frame("Temperature"=src$Temperature[(OBS_NUM-1):38], "Year"=c((2012-38+OBS_NUM-1):2012))

datebreaks <- seq(2008, max(pred$Year)+5, by=2)
#actvspred <- ggplot() + 
ggplot() +  geom_line(data=pred, aes(x=Year, y=Temperature, color="Прогноз Кригинг")) + 
  geom_line(data=predpred, aes(x=Year, y=Temperature, color="Прогноз Тренд")) +
  geom_line(data=act, aes(x=Year, y=Temperature, colour="Актуальное")) +
  scale_x_continuous(breaks=datebreaks) + xlab("Год наблюдения") + ylab("Температура, ºС") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(16,28,1)) +
  labs(color="")
#ggsave(plot=actvspred, file="figures/article/actvspredpred.png", width=7, height=4)