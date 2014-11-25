# Variogram with help of gstat and Saveliev; http://gis-lab.info/docs/saveliev2012-geostat.pdf

library(sp)
#library(gstat)

p <- data.frame("X"=c(1:35), "Y"=rep(1,35))
p.dist<-as.matrix(dist(p[,c("X", "Y")]))
hist(p.dist)
dist.breaks<-quantile(p.dist,seq(0.1, 0.9, by=0.1))
coordinates(p) <- ~ X + Y
p.breaks <- (0:20)*1
hscat(data.res~1, p, breaks=c(0:20))
p.gam_map<-variogram(data.res~1, p, cutoff=25, width=1, map=TRUE) 
plot(p.gam_map, col.regions=gray((16:0)/16))

p.gam<-variogram(Residual~1,p,alpha=0, width=1,cutoff=18) 
plot(p.gam)

model.variog <- vgm(model="Sph", range=1.000031) 
fit.variog <- fit.variogram(p.gam, model.variog) 
plot(p.gam, model=fit.variog) 

# границы сетки - по данным 
x.lim <- range(coordinates(p)[,"X"]) 
y.lim <- range(coordinates(p)[,"Y"]) 
# шаг сетки 
x.step <- 1
y.step <- 0
# позиции линий сетки 
x.pos<-seq(x.lim[1],x.lim[2],by=x.step) 
y.pos<-seq(y.lim[1],y.lim[2],by=y.step) 
# собственно сетка, пока в виде таблицы 
p.grid<-as.data.frame(expand.grid(x.pos,y.pos)) 
names(p.grid)<-c("x","y") 
# сетка как специальная структура gstat 
gridded(p.grid) = ~x+y 
# ординарный кригинг с заданной моделью вариограммы 
e <- krige(Residual~1, p, p.grid, model = fit.variog) 


new <- data.frame(x=c(36:38), y=rep(1, 3)) 
coordinates(new) = ~x+y 
e0 <- krige(Residual~1, p, new, model = fit.variog) 
e0["var1.pred"] 
