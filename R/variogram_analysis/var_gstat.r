# Variogram with help of gstat, sp, nlme; https://beckmw.wordpress.com/tag/variogram/

library(gstat)
library(sp)
library(nlme)  

x <- c(1:35)
y <- rep(1,35)
mod <- lm(Residual~x+y)
coefficients(summary(mod))
dat<-data.frame(x,y,resids=resid(mod))
coordinates(dat) <- ~ x+y
bubble(dat,zcol='resids')
var.mod<-variogram(resids~1,data=dat)
plot(var.mod)