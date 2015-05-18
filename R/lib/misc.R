# Converts years to numerical continuous representation <2010, 2011, 2012> -> <1, 2, 3>
ConvertYearsToNum <- function(years) {
  c(1 : (max(years) - min(years) + 1))
}

# Returns years for which will be prediction calculated
GetPredictionYears <- function (years, number, future, observations) {
  lastYear <- max(years)
  c((lastYear - number + observations - 1) : (lastYear + future))
}

MakeFakeSpatialData <- function (x, data, observations) {
  spdata <- data.frame(cbind("x"=x, "y"=rep(1, observations), data))
  coordinates(spdata) = ~x+y
  return(spdata)
}