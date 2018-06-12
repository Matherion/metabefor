rawTable <- function(..., ncol=2) {
  freqs <- c(...);
  if (!all(is.numeric(freqs))) {
    stop("Non-numeric values specified!");
  }
  return(matrix(c(freqs), ncol=ncol, byrow=TRUE));
}
