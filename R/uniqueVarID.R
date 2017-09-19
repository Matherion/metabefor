uniqueVarID <- function(study, variable) {
  return(paste0(study, "_",
         tolower(gsub('[^[:alnum:]]*', '', variable))));
}