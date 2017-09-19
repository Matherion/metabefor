extractYear <- function(valueOrVector, centuries=c(19,20)) {
  regEx <- paste0(".*((",
                  paste0(seq(min(centuries), max(centuries)), collapse="|"), 
                  ")\\d{2}).*");
  return(sub(regEx, "\\1", valueOrVector));
}