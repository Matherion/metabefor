rxs_parseValueTemplates <- function(valueTemplateDataframe,
                                    valueTemplateCols = valueTemplateColNames()) {
  ### Convert the dataframe to a list
  res <- as.list(valueTemplateDataframe);

  ### Set column names to standard column names
  names(res) <- valueTemplateColNames();

  ### Extract the value template names (identifiers; using [[ because it's a tibble)
  valueTemplateNames <-
    valueTemplateDataframe[[which(names(valueTemplateDataframe) == valueTemplateCols$identifier)]];

  ### Transpose the list using purrr to 'turn it inside out'
  res <- purrr::transpose(res);

  ### Set identifiers as list element names for easy access
  names(res) <- valueTemplateNames;

  ### Store original column names
  attr(res, "originalColNames") <- valueTemplateCols;

  ### Set class and return result
  class(res) <- c("parsedValueTemplates", class(res));
  return(res);
}

print.parsedValueTemplates <- function(x, ...) {
  class(x) <- class(x)[-1];
  print(x, ...);
  invisible(x);
}

