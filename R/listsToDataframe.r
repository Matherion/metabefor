listsToDataframe <- function(listOfLists, identifierColumnPattern = "variable",
                             progress = FALSE) {
  ### Create dataframe to return results
  res <- data.frame();

  if (progress) pBar <- txtProgressBar(style=3,
                                       max = length(listOfLists));
  
  ### Loop through the elements of the list
  ### (associations, univariate data, or variables)
  for (currentList in 1:length(listOfLists)) {
    if (progress) setTxtProgressBar(pBar, currentList);
    ### Loop through the values making up each element
    for (currentValue in names(listOfLists[[currentList]])) {
      ### If this is a single value, store it in the dataframe
      if (is.atomic(listOfLists[[currentList]][[currentValue]]) &&
            length(listOfLists[[currentList]][[currentValue]]) == 1) {
        ### Store it in the dataframe in the right row and column
        res[currentList, currentValue] <-
          listOfLists[[currentList]][[currentValue]];
      }
      else {
        ### Figure out what kind of object it is (vector or a result
        ### of an analysis) and figure out what to do with it. For now,
        ### first skip it.
      }
    }
  }
  if (!is.null(identifierColumnPattern)) {
    ### Store the pattern to identify the identifier columns
    attr(res, "identifierColumnPattern") <- identifierColumnPattern;
    ### Add 'listsToDataframe' to class for correct printing method
    class(res) <- c(class(res), 'listsToDataframe');
  }
  
  ### Return result
  return(res);
}

print.listsToDataframe <- function(x, ...) {
  printableX <- x[, identifierColumns(x)];
  if (is.data.frame(printableX)) {
    print.data.frame(printableX);
  }
  else {
    print(printableX);
  }
  invisible();
}

identifierColumns <- function(sysRevDataFrame) {
  pattern <- as.character(attr(sysRevDataFrame, "identifierColumnPattern"));
  identifierColumns <- grepl(pattern=pattern, x=names(sysRevDataFrame), ignore.case=TRUE);
  res <- names(sysRevDataFrame)[identifierColumns];
  return(res);
}
