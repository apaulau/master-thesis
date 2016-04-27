library(xlsx)
library(ggplot2)
library(lubridate)

df <- read.xlsx2('data/ndb1955-2012.xls', sheetName = 'Баторино_ред', endRow = 1300,
                   colIndex = 1:8, colClasses = c("Date", "character", rep("numeric", 6)))

colnames(df) <- c('date', 'point', 'depth', 'transparency', 'horizont', 'temperature', 'o2solubility', 'saturation')

# Cleaning data

df$point <- trimws(df$point)

## Transform: 'Литораль, ст.1' -> 'Литораль-1'
df$point <- gsub("\\, ст\\. (\\d+)$", "\\-\\1", df$point, ignore.case = TRUE)

## Transform: 'Лит_15_исток р.Дро' -> 'Литораль-15'
df$point <- gsub("^Лит_[а-я\\. ]*(\\d+)(.*)", "Литораль-\\1", df$point)

## Liquidate leading zero
df$point <- gsub("0(\\d)", "\\1", df$point)

## Transform: 'ст.6' -> 'Станция-6'
df$point <- gsub("ст[\\.\\-](\\d+)", "Станция-\\1", df$point, ignore.case = TRUE)

## Transform: Мядель
df$point <- gsub("Мядель-1", "Мядель", df$point, ignore.case = TRUE)
df$point <- gsub("мялель", "Мядель", df$point, ignore.case = TRUE)
df$point <- gsub("мядельская лука", "Мядельская-Лука", df$point, ignore.case = TRUE)

## Transform: 'Литораль-1', 'Пелагиаль-4' -> 'Станция-1', 'Станция-4'
df$point <- gsub("^[а-яА-Я\\-]+(\\d+)$", "Станция-\\1", df$point, ignore.case = TRUE)

df$point <- as.factor(df$point)

## Remove transparency variable as it has so few observations
df$transparency <- NULL


## Extract only data for horizont = 3
batorino.h3 <- df[df$horizont == 3, ]

## Because of data absence in period from start till 1972 extract the rest
batorino.h3 <- batorino.h3[year(batorino.h3$date) > 1972, ]

#$ Дэвис с.110