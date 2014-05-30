vgm1 <- variogram(Residual~1, ~X+Y, p)
fit.variogram(vgm1, vgm(1,"Sph",300,1))

library(geoR)
samp <- as.geodata(cbind(c(1:35),rep(1,35),Residual))
plot(samp)
points(samp)

variog.samp <- variog(samp, max.dist=15)
plot(variog.samp)

ini.vals <- expand.grid(seq(0,1,l=35), seq(0,1,l=35))
ols <- variofit(variog.samp, cov.model="sph",weights="cressie")
