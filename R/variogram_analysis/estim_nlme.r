# Estimating variogram model parameters with nlme. But it seems to be an strange result. http://www.ats.ucla.edu/stat/r/faq/variogram_lme.htm

library(nlme)

spdata <- data.frame(cbind("x"=c(1:35), "y"=rep(1,35), Residual))
null.model <- lme(fixed = Residual ~ 1, data = spdata, random = ~ 1 | Y)
summary(null.model)

cs1Exp <- corExp(1, form = ~ X + Y)
cs1Exp <- Initialize(cs1Exp, spdata)
corMatrix(cs1Exp)[1:10, 1:4]

exp.sp <- update(null.model, correlation = corExp(1, form = ~ X + Y), method = "ML")
summary(exp.sp)

sph.sp <- update(null.model, correlation = corSpher(1, form = ~ X + Y), method = "ML")
summary(sph.sp)