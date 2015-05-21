WriteCharacteristic <- function (expression, type, name) {
  file <- paste(paste("out", type, "characteristics", name, sep="/"), "tex", sep=".")
  writer(format(expression, nsmall=0, digits=3), file)
}

WriteDescriptiveStatistic <- function (expression, type, name) {
  file <- paste(paste("out", type, "descriptive", name, sep="/"), "tex", sep=".")
  writer(format(expression, nsmall=2, digits=3), file)
}

WriteTest <- function (statistic, p.value, df=FALSE, type, name) {
  path <- paste("out", type, "test", name, sep="/")
  writer(format(statistic, nsmall=2, digits=2), paste(path, "statistic.tex", sep="/"))
  writer(format(p.value, nsmall=2, digits=2), paste(path, "p-value.tex", sep="/"))
  if (df) {
    writer(format(df, nsmall=2, digits=2), paste(path, "df.tex", sep="/"))
  }
}

WriteVariogramParams <- function (model, name) {
  WriteCharacteristic(model[[2]][1], type="variogram", name=paste0(name, "-nug"))
  WriteCharacteristic(model[[2]][2], type="variogram", name=paste0(name, "-psill"))
  WriteCharacteristic(model[[3]][2], type="variogram", name=paste0(name, "-range"))
}

writer <- function (expression, file) {
  sink(file=file, type="output")
  cat(expression)
  sink()
}