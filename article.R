## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(xtable)
library(nortest)
#library(aplpack)
library(outliers)
library(tseries)
library(ggplot2)

source("R/misc-fun.R")
source("R/plotting-fun.R")
source("R/print-fun.R")
source("R/dstats.R")

# Reading input data from csv file
data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=35,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

src <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                colClasses = c("Date", "numeric"), stringsAsFactors=F)

src$Date <- c(1:38)
src.fit <- lm(src$Temperature ~ src$Date)
src.res <- src.fit$residuals

data$Date <- c(1:35)
data.fit <- lm(data$Temperature ~ data$Date)
data.res <- data.fit$residuals

data$Date <- c(1:38)
data.pred <- c(data$Temperature, trend(36), trend(37), trend(38))

pred <- data.frame("Temperature"=c(data$Temperature, trend(36), trend(37), trend(38)), "Year"=c(1975:2012))
act <- data.frame("Temperature"=src$Temperature, "Year"=c(1975:2012))

datebreaks <- seq(min(pred$Year)-5, max(pred$Year)+5, by=2)
actvspred <- ggplot() + geom_line(data=pred, aes(x=Year, y=Temperature, color="Прогноз", label="asd")) + geom_line(data=act, aes(x=Year, y=Temperature, colour="Наблюдение"))+
  scale_x_continuous(breaks=datebreaks) + xlab("Год наблюдения") + ylab("Температура, ºС") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(16,28,1)) +
  labs(color="")

ggsave(plot=actvspred, file="figures/article/actvspred.png", width=7, height=4)


hist <- ggplot(data, aes(x=Temperature), geom = 'blank') +   
  stat_function(fun = dnorm, colour='red', 
                arg = list(mean = mean(data$Temperature), sd=sd(data$Temperature)),
                geom = 'line') +                       
  geom_histogram(aes(y = ..density..),colour="darkgrey", fill="white", binwidth=1.25, alpha = 0.4) +
  labs(color="") +
  ylab("Плотность") + xlab("Температура, ºС")
ggsave(plot=hist, file="figures/article/hist.png", width=7, height=4)

data$Date <- c(1975:2009)

# Q-Q plot
qqp <- function (vec) {
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    xlab("Теоретические квантили") + ylab("Выборочные квантили")
}

plot.qq <- qqp(data$Temperature)
ggsave(plot=plot.qq, file="figures/article/qq.png", width=7, height=4)

# Scatterplot
datebreaks <- seq(min(data$Date)-5, max(data$Date)+5, by=2)
plot.scatter <- ggplot(data, aes(x=Date, y=Temperature)) + geom_point() + scale_x_continuous(limits=c(min(data$Date)-5, max(data$Date)+5), breaks=datebreaks) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(10,30,1), limits=c(14,26)) +
  xlab("Год наблюдения") + ylab("Температура, ºС") + geom_abline(intercept=-194.632277, slope=0.107706 )
ggsave(plot=plot.scatter, file="figures/article/scatterplot.png", width=7, height=4)

data.dstats <- dstats.describe(data$Temperature, locale=T)

shapiro <- shapiro.test(data$Temperature)

pearson <- pearson.test(data$Temperature)

# Kolmogorov-Smirnov test for normality
test.nsample <- rnorm(10000, mean=mean(data$Temperature), sd=sd(data$Temperature))
ks <- ks.test(x=data$Temperature, y=test.nsample, exact=NULL)

# Bagplot
to.png(figure.bagplot(data, title="", xlab="Дата", ylab="Температура"),
       "figures/bagplot.png", width=4, height=3)

# Grubbs test for outliers
grubbs <- grubbs.test(data$Temperature)

# Pearson's product-moment correlation test
time <- 1:length(data$Temperature) # for y as numerical
cor.test(data$Temperature, time, method="pearson")

plot.ts <- ggplot(data, aes(x=Date, y=Temperature)) + geom_point() + geom_line() + stat_smooth(method=lm, se=F) + 
  scale_x_continuous(breaks=datebreaks) + xlab("Год наблюдения") + ylab("Температура, ºС") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_y_continuous(breaks=seq(16,28,1))
ggsave(plot=plot.ts, file="figures/article/ts.png", width=7, height=4)

# Getting time series for Temperature. Hack: 1969=1975
data.ts <- ts(data$Temperature, start=c(1969, 7), frequency=1)

# Fitting linear model for data time series
fit <- lm(data.ts ~ time)
data$Residuals <- fit$residuals

# Plot detrended time series
plot.residuals <- ggplot(data, aes(x=Date, y=Residuals)) + geom_point() + geom_line() + stat_smooth(method=lm, se=F) +
  scale_x_continuous(breaks=datebreaks) + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Год наблюдения") + ylab("Температура, ºС")
ggsave(plot=plot.ts, file="figures/article/ts-detrended.png", width=7, height=4)

resdata <- data.frame("Year"=data$Date, "Residual"=data$Residuals)

# Getting descriptive statistics for temperature residuals
resdata.dstats <- dstats.describe(resdata$Residual, locale=T)

# Normal Quantile-Quantile plot for residuals
plot.qq <- qqp(resdata$Residual)
ggsave(plot=plot.qq, file="figures/article/qq-detrended.png", width=7, height=4)

Residual <- resdata$Residual

# Shapiro-Wilk test for normality
resdata.shapiro <- shapiro.test(Residual)

# Pearson chi-square test for normality
resdata.pearson <- pearson.test(Residual)

# Kolmogorov-Smirnov test for normality
test.nsample <- rnorm(10000, mean=mean(Residual), sd=sd(Residual))
resdata.ks <- ks.test(x=Residual, y=test.nsample, exact=NULL)

ggplot(acd, aes(lag, acf)) + geom_bar(stat="identity", width=.05) +
  geom_hline(yintercept=c(0.32, -0.32), linetype="dashed") + 
  scale_y_continuous(limits=c(-0.5,0.5))
ggsave(

# ACF
ci <- .95
acf <- acf(data$Residuals, plot=F, lag.max=30)
acfdf <- data.frame(
  acf = acf$acf,
  lag = acf$lag
)
clim <- qnorm((1 + ci)/2) / sqrt(acf$n.used)

plot.acf <- ggplot(data = acfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(colour = "grey50") + 
  geom_hline(yintercept=c(-clim, clim), linetype="dashed", col="blue") +
  geom_segment(mapping = aes(xend = lag, yend = 0))
ggsave(plot=plot.acf, file="figures/article/acf.png", width=7, height=4)

Box.test(Residual, type="Ljung-Box")
adf.test(Residual)