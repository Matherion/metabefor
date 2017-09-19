generateOperationalisationTemplates <- function(object,
                                                variableName = "variable",
                                                defaultOverrides = NULL,
                                                defaults =
                                                  list(moment = 1,
                                                       type = NA,
                                                       datatype = NA,
                                                       values = NA,
                                                       labels = NA,
                                                       psytype = NA,
                                                       dependent = NA,
                                                       direction = NA,
                                                       parent = NA,
                                                       description = NA,
                                                       comment = NA),
                                                comments =
                                                  list(moment = "moment this variable was measured/manipulated",
                                                       type = '"question", "aggregate", or "manipulation"',
                                                       datatype = '"numeric", "logical", "nominal", "ordinal", or "text"',
                                                       values = 'Possible values; NA or a vector, e.g. c(1, 2, 3)',
                                                       labels = 'Labels for the values; NA or e.g. c("no", "maybe", "sometimes")',
                                                       psytype = '"demographic", "belief", "determinant", "proxy", "behavior"',
                                                       dependent = '"initiation", "cessation"',
                                                       direction = '"consistent" (same direction as dependent), "inconsistent"',
                                                       parent = 'Name of parent variable into which this one is aggregated',
                                                       description = 'Description, as text; or NA',
                                                       comment = 'Comment, as text; or NA'),
                                                outputFile = NULL,
                                                append=TRUE,
                                                encoding="UTF-8",
                                                commentMargin = 2) {
  
  ### Replace defaults by overrides
  for (currentOverride in names(defaultOverrides)) {
    defaults[[currentOverride]] <- defaultOverrides[[currentOverride]];
  }
  
  ### Get a list of all variables for which associations or univariate
  ### results have been extracted.
  variables <- unique(c(unlist(lapply(object$association, function(x) return(c(x$variable1, x$variable2)))),
                        unlist(lapply(object$univariate, function(x) return(x$variable)))));
  
  ### Check whether there are no preceding or trailing spaces (compare to
  ### trimmed version).
  if (any(variables != trim(variables))) {
    stop("Variables '", vecTxt(variables[variables != trim(variables)],
                               delimiter="', '",
                               lastDelimiter="' & '"),
         "' contains preceding or trailing spaces - correct this first!");
  }
  
  ### Generate strings to add before and after the variable names
  preVarName <- 'res$variable[[length(res$variable) + 1]] <- list(\n  variable = "';
  valuesAndDefaults <- paste0("  ", names(defaults),
                              " = ", unlist(defaults), ",");

  ### Remove comma behind last element
  valuesAndDefaults[length(valuesAndDefaults)] <-
    sub(",", "", valuesAndDefaults[length(valuesAndDefaults)], fixed=TRUE);

  ### Add spaces and comments
  fillingSpaces = (max(nchar(valuesAndDefaults)) + commentMargin) - nchar(valuesAndDefaults);
  valuesAndDefaults <-
    paste0(valuesAndDefaults,
           sapply(fillingSpaces, repeatStr, str=" "),
           "### ", unlist(comments), collapse="\n");
  
  postVarName <- paste0('",\n', valuesAndDefaults, "\n);\n");
  
  res <- paste0("###########################################################################
### MEASUREMENTS AND MANIPULATIONS: THE VARIABLES IN THE STUDY
###########################################################################\n\n",
                paste0(preVarName, variables, postVarName, collapse="\n"));
  
  if (is.null(outputFile)) {
    writeClipboard(res);
    cat0("Placed generated template bits on the clipboard (specify",
        "a path and filename in argument 'outputFile' if you want to",
        "write the output directly to a file) for variables ", 
        vecTxt(variables, useQuote="'"), ".");
  } else {
    if (append) {
      fileMode = "a";
    } else {
      fileMode = "w";
    }
    writeLines(paste0("\n", res),
               con=file(description=outputFile,
                        open=fileMode,
                        encoding=encoding));
    cat0("Wrote the generated template bits to file '", outputFile,
         "' (specify NULL for argument 'outpuFile' if you want to ",
         "place the output directly on the clipboard) for variables '", 
         vecTxt(variables, delimiter="', '", lastDelimiter="' & '"),
         "'.");
  }
  
}