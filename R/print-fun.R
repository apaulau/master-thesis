to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if (verbose) {
    cat(sprintf("Creating %s\n", filename));
  }
  dev(filename, ...);
  on.exit(dev.off());
  eval.parent(substitute(expr));
}

to.pdf <- function(expr, filename, ...) {
  to.dev(expr, pdf, filename, useDingbats=FALSE, ...);
}

to.png <- function(expr, filename, ...) {
  to.dev(expr, png, filename, ...);
}