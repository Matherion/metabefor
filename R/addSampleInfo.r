### Add sample variables to dataframe
addSampleInfo <- function(listOfObjects, dataframe, variableToAdd,
                          identifyerColumn='study', append=TRUE) {
  ### If append is false, return only the variable(s) to extract.
  if (append) {
    res <- dataframe;
  } else {
    res <- data.frame();
  }
  ### If variableToAdd is a vector, process elements one by one; if it's
  ### not a vector, it's of course a vector of length one, se we can still
  ### loop through it.
  for (currentVarToAdd in variableToAdd) {
    for (currentRow in 1:nrow(dataframe)) {
      currentIdentifyer <- dataframe[currentRow, identifyerColumn];
      if (is.na(dataframe[currentRow, 'subsample'])) {
        res[currentRow, currentVarToAdd] <-
          ifelse(is.null(listOfObjects[[currentIdentifyer]]$sample[[currentVarToAdd]]),
                 NA, listOfObjects[[currentIdentifyer]]$sample[[currentVarToAdd]]);
      } else {
        currentSubsample <- dataframe[currentRow, 'subsample'];
        if (!is.null(listOfObjects[[currentIdentifyer]]$subsample[[currentSubsample]][[currentVarToAdd]])) {
          res[currentRow, currentVarToAdd] <-
             listOfObjects[[currentIdentifyer]]$subsample[[currentSubsample]][[currentVarToAdd]];
        } else {
          res[currentRow, currentVarToAdd] <-
            ifelse(is.null(listOfObjects[[currentIdentifyer]]$sample[[currentVarToAdd]]),
                   NA, listOfObjects[[currentIdentifyer]]$sample[[currentVarToAdd]]);
        }
      }
    }
  }
  return(res);
}