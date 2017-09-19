importVarClassificFiles <- function(path = getwd(),
                                    filenameRegEx = "variable classification \\((.+)\\).tsv",
                                    classificColNameRegEx = '^classification_\\w$',
                                    classificColName = 'classification',
                                    newClassificColNamesRegEx = "classification_\\1",
                                    uniqueVarID = 'uniqueVarID',
                                    silent=FALSE) {
  
  ### Note: if the colname is specified as regular expression, amatch
  ### is sought, and, if found, this name is retained. If no regular
  ### expression is specified or if no match is found, the newNames
  ### regular expression is used in combination with the filenameRegEx
  ### to generate a column name.
  
  ### Generate object to return results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  res$intermediate$fileList <- list.files(path);
  
  if (!(length(res$intermediate$fileList) > 0)) {
    stop("No files found in specified path '", path, "'.");
  } else if (!silent) {
    cat0("Commencing to inspect ", length(res$intermediate$fileList),
         " files from path '", path, "'.\n");
  }
  
  res$intermediate$filesToRead <- grep(filenameRegEx,
                                       res$intermediate$fileList,
                                       value=TRUE)
  
  if (!(length(res$intermediate$filesToRead) > 0)) {
    stop("No files match regular expression '", filenameRegEx, "'.");
  } else   if (!silent) {
    cat0(length(res$intermediate$filesToRead),
         " files match regular expression '", filenameRegEx, "': ",
         vecTxt(res$intermediate$filesToRead, useQuote="'"),
         ".\n");
  }
  
  res$intermediate$classifiers <-
    sub(filenameRegEx, "\\1", res$intermediate$filesToRead);
  
  res$intermediate$files <- list();
  res$intermediate$classificationColumnNames <- character();
  
  ### Read and process files
  for (currentNumber in 1:length(res$intermediate$filesToRead)) {
    currentFile <- res$intermediate$filesToRead[currentNumber];
    currentClassifyer <- res$intermediate$classifiers[currentNumber];
    res$intermediate$files[[currentFile]] <-
      read.table(file.path(path, currentFile),
                 header = TRUE,
                 sep = "\t",
                 stringsAsFactors = FALSE);
    ### Change column name of the classification colum if no
    ### classificColNameRegEx is specified, or if there is no match
    if (is.null(classificColNameRegEx) ||
          (!any(grepl(classificColNameRegEx,
                      names(res$intermediate$files[[currentFile]]))))) {
      names(res$intermediate$files[[currentFile]])[names(res$intermediate$files[[currentFile]]) == classificColName] <-
        sub("(.*)", newClassificColNamesRegEx, currentClassifyer);
      res$intermediate$classificationColumnNames[currentFile] <-
        sub("(.*)", newClassificColNamesRegEx, currentClassifyer);
    } else {
      res$intermediate$classificationColumnNames[currentFile] <-
        grep(classificColNameRegEx,
             names(res$intermediate$files[[currentFile]]), value=TRUE);
    }
  }
  
  ### Add all unique variable identifying column values together
  uniqueVarIDs <- c();
  for (currentFile in res$intermediate$files) {
    uniqueVarIDs <- c(uniqueVarIDs, currentFile[[uniqueVarID]]);
  }
  
  ### Now we have a complete overview of all variables that were
  ### classified. Now, only select the unique ones, and then merge
  ### the classification columns.
  res$intermediate$uniqueVarIDs <- unique(uniqueVarIDs);
  res$output$dat <- data.frame(uniqueVarID = res$intermediate$uniqueVarIDs);
  
  for (currentFile in length(res$intermediate$files)) {
    colsToMerge <-c(res$intermediate$classificationColumnNames[currentFile],
                    uniqueVarID);
    res$output$dat <-
      merge(res$output$dat,
            res$intermediate$files[[currentFile]][, colsToMerge],
            by=uniqueVarID, all=TRUE);
  }
  
  return(res);
}