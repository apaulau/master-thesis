# Varigram for spatial data with help of geoR; http://www.ats.ucla.edu/stat/r/faq/variogram.htm

library(geoR)

spdata <- data.frame(cbind("X"=c(1:35), "Y"=rep(1,35), Residual))

breaks = seq(0, 18, l = 19)
v1 <- variog(coords = spdata[,1:2], data = spdata[,3], breaks = breaks)

v1.summary <- cbind(c(1:17), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi-variance", "# of pairs")

plot(v1, type = "b", main = "Variogram: Residuals") 
