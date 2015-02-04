# Automap, autofitVariogram, autoKrige

#library(automap)

OBS_NUM <- 35

data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=OBS_NUM,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

data$Date <- c(1:OBS_NUM)
data.fit <- lm(data$Temperature ~ data$Date)
data.res <- data.fit$residuals

spdata <- data.frame(cbind("x"=data$Date, "y"=rep(1,OBS_NUM), data.res))
coordinates(spdata)=~x+y

variogram = autofitVariogram(data.res~1, spdata)
plot(variogram)

new_data <- data.frame("X"=c((OBS_NUM+1):45), "Y"=rep(1,45 - OBS_NUM))
coordinates(new_data) = ~X+Y

k = autoKrige(data.res~1,spdata,new_data)

pred.res <- c(data.res, rep(0,2))
plot(src.res, col='red', type='l', xlim=c(0,45))
#lines(data.res, col="blue")
lines(c((OBS_NUM+1):45), k$krige_output$var1.pred, col='blue', lty=2)

