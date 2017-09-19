exportVarClassificFiles <- function(dat,
                                    variables = NULL,
                                    classifiers = c('A', 'B'),
                                    filename = "variable classification ([RATER]).tsv",
                                    path = getwd(),
                                    classificColNamePrefix = 'classification_',
                                    classificColNr = 2,
                                    raterVarName = 'rater') {
  
  ### Generate object to return results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### If any classiciation columns exist already (from previous
  ### classification efforts), add them to the list of variables
  ### to keep.
  res$intermediate$variables <- c(variables,
                                  paste0(classificColNamePrefix, classifiers)[paste0(classificColNamePrefix, classifiers) %IN% names(dat)]);
  
  ### Extract variables to write to the file
  if (is.null(variables)) {
    res$intermediate$dat <- unique(dat);
    res$intermediate$variables <- names(dat);
  } else {
    res$intermediate$dat <- unique(dat[, res$intermediate$variables]);
  }
  
  if (classificColNr < 2) {
    res$intermediate$dat <- cbind(rep("", nrow(res$intermediate$dat)),
                                  res$intermediate$dat);
    names(res$intermediate$dat)[1] <- classificColName;
  } else {
    res$intermediate$dat <- cbind(res$intermediate$dat[1:(classificColNr-1)],
                                  rep("", nrow(res$intermediate$dat)),
                                  res$intermediate$dat[classificColNr:ncol(res$intermediate$dat)]);
  }
  
  for (currentRater in classifiers) {
    tmpDataFrame <- res$intermediate$dat;
    tmpDataFrame[[raterVarName]] <- rep(currentRater, nrow(res$intermediate$dat));
    currClassificColName <- paste0(classificColNamePrefix, currentRater);
    if (currClassificColName %IN% names(tmpDataFrame)) {
      ### If we already have a column with ratings, replace the empty
      ### column for the classifications with the old ones (first
      ### rename the old one)
      names(tmpDataFrame)[names(tmpDataFrame)==currClassificColName] <-
        paste0(currClassificColName, "_", format(Sys.time(), "%Y%m%d_%H%M"));
      tmpDataFrame[[classificColNr]] <-
        tmpDataFrame[[paste0(currClassificColName, "_", format(Sys.time(), "%Y%m%d_%H%M"))]];
      names(tmpDataFrame)[classificColNr] <- currClassificColName;
    } else {
      ### Otherwise, change its name to designate it for this classifier's
      ### classifications
      names(tmpDataFrame)[classificColNr] <- currClassificColName;
    }
    
    ### Store in object to return
    res$output[[currentRater]] <- tmpDataFrame;
    
    if ((tolower(substring(filename, nchar(filename) - 2))) == "tsv") {
      write.table(tmpDataFrame,
                  file.path(path,
                            sub('[RATER]', currentRater, filename, fixed=TRUE)),
                  sep="\t", row.names=FALSE, na="");
    } else if ((tolower(substring(filename, nchar(filename) - 2))) == "xls") {
      capture.output(print(xtable(tmpDataFrame), type="html"),
                     file=file.path(path,
                                    sub('[RATER]', currentRater, filename, fixed=TRUE)));
    }
  }
  
  invisible(res);
}