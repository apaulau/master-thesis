## Analysis for lake Batorino, sample consist of observations of water temperature for each July month from 1975 to 2012.
## batorino_july.csv is csv file with observed data.
## First column is Date in YYYY/MM/DD format.
## Second column is observed temperature value.

# For antialiasing
library(Cairo);
CairoWin();

# Reading input data from csv file
data <- read.csv(file="batorino_july.csv", header=T, sep=";", nrows=38,
                 colClasses = c("Date", "numeric"), stringsAsFactors=F);

print(data);

# Plotting received data
CairoPNG(file="~/out/time-series.pdf")
plot(data, pch=21,  col='darkgrey', bg='grey', main="Temperature Time Series");
lines(data, lwd=1, col="darkred");
dev.off();


library(psych)
dstats <- describe(data$Temperature);
print(dstats);
