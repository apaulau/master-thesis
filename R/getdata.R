library(xlsx)
library(lubridate)

# ggplot(dataset) + geom_line(aes(date, temperature, col = saturation))
read <- function() {
    filename <- 'data/ndb1955-2012.xls'

    df <- read.xlsx2(filename, sheetName = 'Баторино_ред', endRow = 1300, colIndex = 1:8, colClasses = c("Date", "character", rep("numeric", 6)))
    colnames(df) <- c('date', 'point', 'depth', 'transparency', 'horizont', 'temperature', 'o2solubility', 'saturation')

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

    ## As we extracted data with horizont=3 there's no need in this variable
    batorino.h3$horizont <- NULL

    ## Also maybe there is no sense in point variable as it could the same sensor but with different name
    batorino.h3$point <- NULL

    ## Rounding dates down to the nearest month for further aggregating (because we wanna study data by months)
    batorino.h3$date <- floor_date(batorino.h3$date, "month")
    batorino.aggregated <- aggregate(batorino.h3, by = list(group = batorino.h3$date), mean)
    batorino.aggregated$group <- NULL

    whichMonthMostVoluminous <- function(df) {
        observationsByMonth <- sapply(1:12, function(x) dim(df[month(df$date) == x, ])[1])
        return(which.max(observationsByMonth))
    }

    mostVoluminousMonth <- whichMonthMostVoluminous(batorino.aggregated)
    dataset <- batorino.aggregated[month(batorino.aggregated$date) == mostVoluminousMonth, ]

    ## I found that there is gap between 73 and 75 years. So it seems we can have mean mean of neighborhood months for this year
    getApproximateJulyRow <- function(df, year) {
        ## Get observations in provided year
        tmp <- df[year(df$date) == year, ]

        ## Get mean between june and august
        july <- data.frame(rbind(colMeans(tmp[(month(tmp$date) == 06 | month(tmp$date) == 08), -1])))
        july$date <- date(paste0(year, '-07-01'))

        return(july)
    }

    approximateJuly1974 <- getApproximateJulyRow(batorino.aggregated, 1974)

    ## Add approximate row for July, 1974 to dataset
    dataset <- data.frame(rbind(dataset, approximateJuly1974))

    ## Sort it by date
    dataset <- dataset[order(dataset$date),]

    rownames(dataset) <- 1:dim(dataset)[1]

    dataset <- cbind(year = year(dataset$date), dataset)
    dataset$date <- NULL
    dataset$depth <- NULL

    return(dataset)
}
