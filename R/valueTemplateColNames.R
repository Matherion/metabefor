valueTemplateColNames <- function(...) {
  res <- c(identifierCol = "identifier",
           descriptionCol = "description",
           validValuesCol = "validValues",
           defaultCol = "default",
           examplesCol = "examples",
           validationCol = "validation",
           errorCol = "error");
  tweaks <- unlist(list(...));
  presentElements <- names(tweaks) %in% names(res);
  if (!all(presentElements)) {
    warning(paste0("Some column names (",
                   vecTxtQ(names(tweaks)[!presentElements]),
                   ") are invalid! Ignoring these."));
    tweaks <- tweaks[presentElements];
  }
  res[names(tweaks)] <- tweaks;
  res <- as.list(res);
  return(res);
};
