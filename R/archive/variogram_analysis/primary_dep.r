lens <- 1:30
res1 <- c()
res2 <- c()
i <- 1
for(l in lens) {
  variogram1 = autofitVariogram1(data.res~1, spdata, len=l)
  variogram2 = autofitVariogram(data.res~1, spdata, len=l)
  res1[i] <- variogram1$sserr/l
  res2[i] <- variogram2$sserr/l
  i = i +1
}
df1 <- data.frame("X"=lens, "Y"=res1)
df2 <- data.frame("X"=lens, "Y"=res2)
ggplot(df1, aes(x=X,y=Y, color="classic")) + geom_line() + geom_line(data=df2, aes(x=X,y=Y,color="cressie")) + scale_x_continuous(breaks=lens)
