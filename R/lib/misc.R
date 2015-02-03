# Converts years to numerical continuous representation <2010, 2011, 2012> -> <1, 2, 3>
ConvertYearsToNum <- function(years) {
  c(1 : (max(years) - min(years)))
}

# Returns years for which will be prediction calculated
GetPredictionYears <- function (years, number, future) {
  lastYear <- max(years)
  c((lastYear - number + kObservationNum - 1) : (lastYear + future))
}