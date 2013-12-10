date.year.add <- function(date, count) {
  d <- as.POSIXlt(date)
  d$year <- d$year + count
  as.Date(d)
}

date.year.subtract <- function(date, count) {
  date.year.add(date, -count)
}

date.wrap <- function(min_date, Date, max_date) {
  Date <- append(seq.Date(min_date, min(Date), "years"), Date)
  Date <- append(Date, seq.Date(max(Date), max_date, "years"))
}

residuals.get <- function(residuals) {
  count <- 1
  length <- length(residuals)
  result <- vector()
  while(count <= length) {
    result[count] <- residuals[[count]]
    count <- count + 1
  }
  result
}