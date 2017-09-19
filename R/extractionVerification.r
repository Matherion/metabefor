extractionVerification <- function(extractionObject, variableOverview = FALSE) {
  if (class(extractionObject) != 'sysRevExtractionObject') {
    stop("The provided object is not of class 'sysRevExtractionObject' (which is required)!");
  }
  
  ### Object to store results
  res <- list(dat = list(), variableOverview = variableOverview);
  
  ### Construct dataframes with extracted stuff and
  ### extract lists of variables in each dataframe
  if (!is.null(extractionObject$variable) & length(extractionObject$variable) > 0) {
    res$dat$extractedVariables <- listsToDataframe(extractionObject$variable);
    res$describedVariables <-
      c(unlist(res$dat$extractedVariables[, identifierColumns(res$dat$extractedVariables)]));
  }
  else {
    stop("No operationalisations (measurements and/or manipulations) extracted in this extraction object!");
  }
  if (length(extractionObject$univariate) > 0) {
    res$dat$extractedUnivariate <- listsToDataframe(extractionObject$univariate);
    res$usedVariables <-
      c(unlist(res$dat$extractedUnivariate[, identifierColumns(res$dat$extractedUnivariate)]));
  }
  if (length(extractionObject$association) > 0) {
    res$dat$extractedAssociations <- listsToDataframe(extractionObject$association);
    if (length(extractionObject$univariate) > 0) {
      res$usedVariables <- c(res$usedVariables,
                             c(unlist(res$dat$extractedAssociation[, identifierColumns(res$dat$extractedAssociation)])));
    }
    else {
      res$usedVariables <-
        c(c(unlist(res$dat$extractedAssociation[, identifierColumns(res$dat$extractedAssociation)])));
    }
  }
  
  ### Eliminate duplicates
  res$usedVariables <- unique(res$usedVariables);
  
  ### List omitted variables
  res$usedNotDescribed <- res$usedVariables[!(res$usedVariables %in% res$describedVariables)];
  res$describedNotUsed <- res$describedVariables[!(res$describedVariables %in% res$usedVariables)];
  
  ### Set class and return result
  class(res) <- "extractionVerification";
  return(res);
}

print.extractionVerification <- function(x, variableOverview = x$variableOverview, ...) {
  if (variableOverview) {
    cat("\n###### EXTRACTED VARIABLES: ######\n");
    print(x$dat$extractedVariables);
    cat("\n###### EXTRACTED UNIVARIATE DATA: ######\n");
    print(x$dat$extractedUnivariate);
    cat("\n###### EXTRACTED ASSOCIATIONS: ######\n");
    print(x$dat$extractedAssociation);
    cat("\n");
  };
  if ((length(x$usedNotDescribed) > 0) | (length(x$describedNotUsed) > 0)) {
    cat("###### WARNINGS: ######\n");
    if (length(x$describedNotUsed) > 0) {
      cat(paste0("Variables of which the operationalisation is extracted, ",
                 "but for which no associations or univariate results are extracted:\n  ",
                 paste0(x$describedNotUsed, collapse=", "), "\n"));
      
    }
    if (length(x$usedNotDescribed) > 0) {
      cat(paste0("Variables of which associations or univariate results are extracted, ",
                 "but of which the operationalisation is not extracted:\n",
                 '  variable = "',
                 paste0(x$usedNotDescribed, collapse='",\n  variable = "'),
                 '",\n'));
    }
  }
  else {
    cat("SysRev extraction object verified: no inconsistencies found!\n");
  }
  if (!(variableOverview)) {
    cat("(use 'variableOverview=TRUE' to display the names of all extracted variables)");
  }
  invisible();
}
