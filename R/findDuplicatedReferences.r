findDuplicateReferences <- function(primaryRefs, secondaryRefs,
                                    fields = c('title', 'year', 'author'),
                                    exact = FALSE, duplicateCriterion=NULL,
                                    minLength = 4, split = "[^a-zA-Z0-9]",
                                    duplicateField = "isDuplicate",
                                    duplicateFieldValue = "duplicate",
                                    recordLogField = "recordLog",
                                    newRecordValue = "new",
                                    duplicateValue = "duplicate",
                                    originalValue = "original",
                                    originFilenameField="originFilename",
                                    silent = FALSE){
  
  res <- list(input = list(primaryRefs = primaryRefs,
                           secondaryRefs = secondaryRefs,
                           fields = fields, exact = exact,
                           minLength = minLength, split = split,
                           duplicateCriterion = duplicateCriterion,
                           duplicateField = duplicateField,
                           recordLogField = recordLogField,
                           newRecordValue = newRecordValue,
                           duplicateValue = duplicateValue,
                           originalValue = originalValue,
                           originFilenameField = originFilenameField,
                           silent = silent),
              intermediate = list(secondaryRefRecords = secondaryRefs$output$records),
              output = list());
  
  ### If not specified how many duplicate fields cause a record to be
  ### considered 'duplicate', take the number of fields
  if (is.null(duplicateCriterion)) {
    res$intermediate$duplicateCriterion <- length(fields);
  }
  else {
    res$intermediate$duplicateCriterion <- duplicateCriterion;
  }
  
  ### Extract columns to compare
  res$intermediate$columnsToCompare <- list(primaryRefs = primaryRefs$output$records[, fields],
                                            secondaryRefs = secondaryRefs$output$records[, fields]);

  ### Convert factors to text strings (character class)
  res$intermediate$columnsToCompare$primaryRefs <- 
    as.data.frame(lapply(res$intermediate$columnsToCompare$primaryRefs, function(vector) {
                    if (is.factor(vector)) {
                      return(as.character(vector));
                    }
                    else {
                      return(vector);
                    }
                  }), stringsAsFactors=FALSE);
  res$intermediate$columnsToCompare$secondaryRefs <- 
    as.data.frame(lapply(res$intermediate$columnsToCompare$secondaryRefs, function(vector) {
      if (is.factor(vector)) {
        return(as.character(vector));
      }
      else {
        return(vector);
      }
    }), stringsAsFactors=FALSE);
  
  
  ### If not looking for exact matches, process columns
  if (!exact) {
    res$intermediate$columnsToCompare$primaryRefs <-
      as.data.frame(lapply(res$intermediate$columnsToCompare$primaryRefs,
                           simplifyString, minLength = minLength, split = split),
                    stringsAsFactors=FALSE);
    res$intermediate$columnsToCompare$secondaryRefs <-
      as.data.frame(lapply(res$intermediate$columnsToCompare$secondaryRefs,
                           simplifyString, minLength = minLength, split = split),
                    stringsAsFactors=FALSE);
  }
  
  ### Show basic information and store datetime.
  if (!silent) {
    startTime <- Sys.time();
    cat(paste0("Merging bibliographic databases and flagging duplicates. Processing ",
               nrow(res$intermediate$columnsToCompare$primaryRefs),
               " primary references and ",
               nrow(res$intermediate$columnsToCompare$secondaryRefs),
               " secondary references. It is now ", startTime, ".\n"));
  
    duplicatedReferencesProgressBar <- txtProgressBar(style=3,
                                                      min=1,
                                                      max=nrow(res$intermediate$columnsToCompare$secondaryRefs),
                                                      initial=1);
  }
  
  ### Process every record in the secondary records list, and compare it
  ### to each record in the primary records list. For each field, store
  ### whether it is the same.
  res$intermediate$duplicateMatches <- vector();
  for (currentSecondaryRecord in 1:nrow(res$intermediate$columnsToCompare$secondaryRefs)) {
    ### Create a temporary dataframe, where we store, for each primary record
    ### that we process in the next loop, and for each field that we have to
    ### check, whether the field is a duplicate.
    tempDataFrame <- data.frame();
    ### Process all primary records
    for (currentPrimaryRecord in 1:nrow(res$intermediate$columnsToCompare$primaryRefs)) {
      ### Compare all fields
      for (currentField in fields) {        
        if (!is.na(res$intermediate$columnsToCompare$secondaryRefs[currentSecondaryRecord, currentField])) {
          ### Store, for this field, whether the secondary and primary record
          ### have the same value.
          tempDataFrame[currentPrimaryRecord, currentField] <-
            res$intermediate$columnsToCompare$secondaryRefs[currentSecondaryRecord, currentField] ==
            res$intermediate$columnsToCompare$primaryRefs[currentPrimaryRecord, currentField];
        }
        else {
          if (is.na(res$intermediate$columnsToCompare$primaryRefs[currentPrimaryRecord, currentField])) {
            ### Both missing, so consider it a duplicate
            tempDataFrame[currentPrimaryRecord, currentField] <- TRUE;
          }
          else {
            ### In the primary record, this field is not missing, so no duplicate
            tempDataFrame[currentPrimaryRecord, currentField] <- FALSE;
          }
        }
      }
      if (!silent) {
        setTxtProgressBar(duplicatedReferencesProgressBar, currentSecondaryRecord);
      }
    }
    ### Now, sum the number of 'TRUEs' for each primary record (within this
    ### secondary record). In other words, how many fields does this secondary
    ### record share with each primary record?
    sharedFields <- rowSums(tempDataFrame);
    
    ### Now, take the maximum number of shared fields. This is the value
    ### that determines whether this secondary record has any duplicates.
    res$intermediate$duplicateMatches[currentSecondaryRecord] <- max(sharedFields, na.rm=TRUE);
  }
  
  ### Store vector indicating whether a record is new, based on the
  ### vector with number of fields where there are duplicate hits and
  ### the threshold in duplicateCriterion
  res$intermediate$newRecord <-
    res$intermediate$duplicateMatches < res$intermediate$duplicateCriterion;
  ### ... And store vector with duplicate records
  res$intermediate$duplicateRecord <- !res$intermediate$newRecord;
  
  ### Indicate duplicate status
  res$intermediate$secondaryRefRecords[!res$intermediate$newRecord, duplicateField] <-
    duplicateFieldValue;
  
  ### Add current status to record log field
  if (recordLogField %in% names(res$intermediate$secondaryRefRecords)) {
    ### The column already exists, so just add the current activity
    res$intermediate$secondaryRefRecords[res$intermediate$newRecord, recordLogField] <-
      paste(res$intermediate$secondaryRefRecords[res$intermediate$newRecord, recordLogField],
            "->", newRecordValue);
    res$intermediate$secondaryRefRecords[!res$intermediate$newRecord, recordLogField] <-
      paste(res$intermediate$secondaryRefRecords[!res$intermediate$newRecord, recordLogField],
            "->", duplicateValue);
  }
  else {
    ### No column yet, so we create one.
    res$intermediate$secondaryRefRecords[res$intermediate$newRecord, recordLogField] <-
      newRecordValue;
    res$intermediate$secondaryRefRecords[!res$intermediate$newRecord, recordLogField] <-
      duplicateValue;
  }
  
  ### Store original filename
  res$intermediate$secondaryRefRecords[, originFilenameField] <-
    res$input$secondaryRefs$input$filename;
  
  ### Store output: the new records separately, and the combination of
  ### primary and secondary records
  res$output$newRecords <-
    res$intermediate$secondaryRefRecords[res$intermediate$newRecord, ];
  
  ### Store primary records
  res$output$records <- res$input$primaryRefs$output$records;
  
  ### Add record status for primary records
  if (recordLogField %in% names(res$output$records)) {
    ### The column already exists, so just add the current status
    res$output$records[, recordLogField] <-
      paste(res$output$records[, recordLogField], "->", originalValue);
  }
  else {
    ### No column yet, so we create one.
    res$output$records[, recordLogField] <- originalValue;
  }
  
  ### Combine the primary and secondary records
  res$output$records <- rbind.fill(res$output$records,
                                   res$intermediate$secondaryRefRecords);
  
  ### Show results
  if (!silent) {
    endTime <- Sys.time();
    cat(paste0("\nProcessed ", nrow(res$input$primaryRefs$output$records),
               " primary and ", nrow(res$input$secondaryRefs$output$records),
               " secondary records. Identified ", sum(res$intermediate$duplicateRecord),
               " duplicate records. Total number of records in resulting set is ",
               nrow(res$output$records), ", of which ",
               sum(match(res$output$records[, duplicateField], duplicateFieldValue), na.rm=TRUE),
               " duplicates. It is now ", endTime, ". "));
    cat(paste0("The process took roughly: ", capture.output(print(difftime(endTime, startTime))), "."));
  }
  
  ### Return results
  return(res)
  
}

simplifyString <- function(textString, minLength = 4, split="[^a-zA-Z0-9]") {
  ### Split by all non-alphanumeric characters, extract length
  ### of every fragment, compare to minLength, remove all
  ### fragments that are too short, paste remaining fragments
  ### together, and unlist back to a character vector
  
  if (!is.character(textString)) {
    stop("The textString argument of simplifyString must be of type 'character' (i.e. a text string). The provided argument has class: ",
         class(textString));
  }
  
  textString <- unlist(lapply(strsplit(textString, split), function(x) {
    wordLengths <- nchar(x);
    return(paste(x[wordLengths>=minLength], collapse=""));
  }))
  ### Return result
  return(tolower(trim(as.character(textString))));
}

