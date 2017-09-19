buildScreenerPackage <- function(libraryObject,
                                 screeners = c("a", "b"),
                                 screenerFieldsPrefix = "screener",
                                 screenerFieldsSuffix = "status",
                                 duplicateField = NULL,
                                 outputPath = getwd(),
                                 basename = "screening_",
                                 screeningType = "screening",
                                 suffixedConfigFiles = FALSE,
                                 ...) {
  
  res<- list(input = c(list(call = sys.call()),
                       as.list(environment()),
                       list(sysCall = as.list(sys.call()))),
             intermediate = list(),
             output = list());
  
  ### Check whether we have information about duplicates
  if (!is.null(duplicateField) && (duplicateField %in% names(libraryObject$output$records))) {
    ### Store whether a record is a duplicate in the screener fields
    for (currentScreener in 1:length(screeners)) {
      libraryObject$output$records[, paste0(screenerFieldsPrefix, screeners[currentScreener], screenerFieldsSuffix)] <-
        libraryObject$output$records[, duplicateField];
    }
  }
  
  res$intermediate$screenerDirs <- list();
  res$intermediate$screenerLibraryNames <- list();
  for (currentScreener in 1:length(screeners)) {
    res$intermediate$screenerDirs[[currentScreener]] <- paste0(outputPath, "/",
                                                   basename, screeners[currentScreener]);
    res$intermediate$screenerLibraryNames[[currentScreener]] <-
      paste0(basename, screeners[currentScreener], ".bibtex");
    
    ### Create directory if it doesn't exist yet
    if (!file.exists(res$intermediate$screenerDirs[[currentScreener]])) {
      dir.create(res$intermediate$screenerDirs[[currentScreener]]);
    }
    
    ### Store library
    sysrevExport(libraryObject,
                 filename=paste0(res$intermediate$screenerDirs[[currentScreener]], "/",
                                 res$intermediate$screenerLibraryNames[[currentScreener]]),
                 screeningType="screening");
    ### Generate Jabref configuration files
    configFiles <- buildJabRefConfigFiles(screeners = screeners[currentScreener],
                                          screenerFieldsPrefix = screenerFieldsPrefix,
                                          screenerFieldsSuffix = screenerFieldsSuffix,
                                          outputPath = res$intermediate$screenerDirs[[currentScreener]],
                                          ...);
  }
  
  return(res);

}