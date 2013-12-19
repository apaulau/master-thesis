to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if (verbose) {
    cat(sprintf("Creating %s\n", filename));
  }
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ...) {
  #to.dev(expr, pdf, filename, useDingbats=FALSE, encoding="UTF-8", ...)
  cairo_pdf(filename, ...)
  expr
  dev.off()
}

to.png <- function(expr, filename, ...) {
  to.dev(expr, png, filename, ...)
}

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