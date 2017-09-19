### Add variable information to dataframe
addVariableInfo <- function(variablesDataframe, dataframe, variableToAdd,
                            identifyerColumn='study',
                            variableNameColumn='variable',
                            variableNameColumnInDF='variable',
                            newVarPrefix="",
                            append=TRUE, silent=FALSE,
                            superDetailed=FALSE) {
  ### If append is false, return only the variable(s) to extract.
  if (append) {
    res <- dataframe;
  } else {
    res <- dataframe <- data.frame();
  }
  ### If variableToAdd is a vector, process elements one by one; if it's
  ### not a vector, it's of course a vector of length one, se we can still
  ### loop through it.
  for (currentVarToAdd in variableToAdd) {
    ### First check whether this is a character or a numeric variable;
    ### then create it in the dataframe.
    if (is.character(variablesDataframe[[currentVarToAdd]])) {
      if (!silent) {
        cat0("Adding character variable '", currentVarToAdd, "'.\n");
      }
      dataframe[[currentVarToAdd]] <- as.character(rep(NA,  nrow(dataframe)));
    } else if (is.numeric(variablesDataframe[[currentVarToAdd]])) {
      if (!silent) {
        cat0("Adding numeric variable '", currentVarToAdd, "'.\n");
      }
      dataframe[[currentVarToAdd]] <- as.numeric(rep(NA,  nrow(dataframe)));
    }
    ### Loop through dataframe row by row
    if (nrow(dataframe) > 0) {
      for (currentRow in 1:nrow(dataframe)) {
        ### Store to which study this row belongs (i.e. the value of the
        ### identifyer, which will usually be study name or smth)
        currentIdentifyer <- as.character(dataframe[currentRow, identifyerColumn]);
        ### Store for which variable we're extracting information
        currentVariable <- as.character(dataframe[currentRow, variableNameColumnInDF]);

        if (superDetailed) {
         cat0("currentIdentifyer=", currentIdentifyer,
               ", currentVariable=", currentVariable, ", currentRow=",
               currentRow, ", rows in variable dataframe that apply: ",
              sum((variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
                    (variablesDataframe[[variableNameColumn]] == currentVariable)),
              "\n");
          
#           cat("------------------------\n");
#           cat0(currentVarToAdd, " = ", variablesDataframe[
#             (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
#               (variablesDataframe[[variableNameColumn]] == currentVariable),
#             currentVarToAdd], "\n");
#            cat("------------------------\n");
#           cat0("length:", length(variablesDataframe[
#             (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
#               (variablesDataframe[[variableNameColumn]] == currentVariable),
#             currentVarToAdd]), "\n");
#           cat("------------------------\n");
        }
        
        ### For all values (including NA), ...
        if ((!is.null(variablesDataframe[
          (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
            (variablesDataframe[[variableNameColumn]] == currentVariable),
          currentVarToAdd])) &&
            (length(variablesDataframe[
              (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
                (variablesDataframe[[variableNameColumn]] == currentVariable),
              currentVarToAdd]) == 1)) {
          ### ... extract the value from the variables dataset and add it
          ### to whichever dataset we're adding stuff to.
          res[currentRow, paste0(newVarPrefix, currentVarToAdd)] <-
            tryCatch(variablesDataframe[
              (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
                (variablesDataframe[[variableNameColumn]] == currentVariable),
              currentVarToAdd], error = function(e) {
                if (!silent) {
                  cat0("Encountered error trying to add measurement or manipulation",
                       " (operationalisation) detail '", currentVarToAdd,
                       "' to '", currentVariable,
                       "' for study '", currentIdentifyer, "'. Error R provided:\n",
                       paste0(e));
                  print(variablesDataframe[
                    (variablesDataframe[[identifyerColumn]] == currentIdentifyer) &
                      (variablesDataframe[[variableNameColumn]] == currentVariable),
                    currentVarToAdd]);
                  print(res[, currentVarToAdd]);
                }
              });
          }
        }
      }
    }
    return(res);
  }