to.file <- function(expr, filename) {
  sink(file=filename, type="output")
  if (class(expr)[1] == "htest") {
    to.verbatim(expr)
  } else {
    print(expr)  
  }
  sink()
}

to.verbatim <- function(expr) {
  cat("\\begin{verbatim}", "\n")
  print(expr)
  cat("\\end{verbatim}")
}