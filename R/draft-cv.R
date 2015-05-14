library(ggvis)
library(dplyr)
library(tseries)
library(sp)
library(gstat)
library(reshape2)
df <- data.frame("d"=research.data.residuals, "x"=c(1:kObservationNum), "y"=rep(1, kObservationNum))
coordinates(df) <- ~x+y

out <- krige.cv(d~1, df, variogram.manual$var_model, nfold=32)

# mean error, ideally 0
mean(out$residual)
# MSPE ideally small
mean(out$residual^2)
# Mean square normalized error, ideally close to 1
mean(out$zscore^2)

# correlation observed and predicted, ideally 1
cor(out$observed, out$observed - out$residual)
# correlation predicted and residual, ideally 0
cor(out$observed - out$residual, out$residual)

out <- krige.cv(d~1, df, variogram.robust.best$var_model, nfold=32)

cor(out$observed - out$residual, out$residual)
cor(out$observed, out$observed - out$residual)

v <- ComputeVariogram(data=c(1,2,3,4,5,6,7,8), x=c(1:8), cressie=FALSE, cutoff=6, width=FALSE)
df <- data.frame("d"=c(1,2,3,4,5,6,7,8), "x"=c(1:8), "y"=rep(1, 8))
coordinates(df) <- ~x+y
out <- krige.cv(d~1, df, v$var_model, nfold=8)
