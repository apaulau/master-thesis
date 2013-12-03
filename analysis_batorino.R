## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

rm(list=ls(all=TRUE)) #start with empty workspace

library(xtable)
library(nortest)
library(aplpack)
library(outliers)
source("R/plotting-fun.R")
source("R/print-fun.R")
source("R/dstats.R")

# Reading input data from csv file
data <- read.csv(file="data/batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F)

print(data)

Temperature <- data$Temperature

# Plotting received data
to.pdf(figure.ts(Temperature, title="", ylab="Temperature"),
       "figures/temperature-ts-first-overview.pdf",
       width=6, height=4);


# Plotting histogram for temperature variable
to.pdf(figure.hist(Temperature, "Histogram of Temperature"), 
       "figures/temperature-histogram.pdf", width=6, height=4);

# Plotting histogram with fitted normal density curve for temperature variable
to.pdf(figure.hist(Temperature, "Histogram with fitted normal density curve", freq=F, dnorm), 
       "figures/temperature-histogram-dnorm.pdf", width=6, height=4);

# Getting descriptive statistics for temperature
dstats <- dstats.describe(Temperature, locale=T)

# Output descriptive statistics to TeX
to.file(xtable(dstats, caption="Описательные статистики для наблюдаемых температур.", label="table:dstats"),
     "out/dstats.tex")

# Normal Quantile-Quantile plot
to.pdf(figure.qqnorm(Temperature),
       "figures/temperature-qqnorm.pdf", width=6, height=4)

# Shapiro-Wilk test for normality
shapiro <- shapiro.test(Temperature)
# Output results to TeX
to.file(shapiro, "out/shapiro.tex")

# Pearson chi-square test for normality
pearson <- pearson.test(Temperature)
# Output results to TeX
to.file(pearson, "out/pearson.tex")

# Kolmogorov-Smirnov test for normality
test.nsample <- rnorm(10000, mean=mean(Temperature), sd=sd(Temperature))
ks <- ks.test(x=Temperature, y=test.nsample, exact=NULL)
# Output results to TeX
to.file(ks, "out/ks.tex")

to.pdf(figure.bagplot(data),
       "figures/bagplot.pdf", width=6, height=4)

# Grubbs test for outliers
grubbs <- grubbs.test(Temperature)
# Output results to TeX
to.file(grubbs, "out/grubbs.tex")

# Correlation matrix
cmatrix <- cor(cbind(Temperature, "Date"=1:length(Temperature)), method="pearson")
to.file(xtable(cmatrix, caption="Корреляционная матрица.", label="table:cmatrix"),
        "out/cmatrix.tex")