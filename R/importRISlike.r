importRISlike <- function(filename, hasBOM = FALSE,
                          encoding=NULL,
                          convertFromUTF = TRUE,
                          silent = FALSE, ...) {

  if (!silent) cat0("Reading file '", filename, "'...\n");

  ### Read lines from bibtex file
  if (hasBOM) {
    con <- file(filename, "r", encoding="UTF-8-BOM");
    sourceLines <- readLines(con, ...);
  }
  else if (is.null(encoding)) {
    con <- file(filename, "r");
    sourceLines <- readLines(con, ...);
  }
  else {
    con <- file(filename, "r", encoding=encoding);
    sourceLines <- readLines(con,  ..., encoding=encoding);
  }
  close(con);

  ### Sometimes, reading the firstline using hasBOM does not work. In those
  ### situations, the three characters together forming the Byte Order Mark
  ### are still present in the first line. Make sure they're removed before
  ### we convert to UTF-8.
  sourceLines[1] <- sub(".*(TY  - .*)", "\\1", sourceLines[1]);

  ### Convert to UTF-8
  if (convertFromUTF) {
    sourceLines <- iconv(sourceLines, from="UTF-8");
  }

  ### Generate object to return
  res <- list(input = list(filename = filename,
                           encoding = encoding),
              intermediate = list(sourceLines = sourceLines),
              output = list());

  if (!silent) cat0("Read ", length(sourceLines), " lines.\n");

  ### The Ebcohost output may start with some weird symbols.
  ### If the first line contains "TY  - ", delete everything in front of it.
  sourceLines[1] <- sub(".*(TY  - .*)", "\\1", sourceLines[1]);

  res$intermediate$firstLinesOfRecords <- grep("^TY", sourceLines);
  #res$intermediate$lastLinesOfRecords <- grep("^ER", sourceLines) - 1;
  res$intermediate$exportSource <- 'ris';

  if (!silent) {
    cat0("Extracted ", length(res$intermediate$firstLinesOfRecords),
         " lines matching regex '^TY' (regular RIS start of record).\n");
    #cat0("Extracted ", length(res$intermediate$lastLinesOfRecords), " lines matching regex '^ER' (regular RIS end of record).\n");
  }

  if (length(res$intermediate$firstLinesOfRecords) == 0) {
    ### No RIS record opening or closing tags. This might be a pubmed
    ### exported file (uses almost the same tag dictionary, but
    ### slightly different, including different opening and closing tags)
    res$intermediate$firstLinesOfRecords <- grep("^PMID",sourceLines);
    #res$intermediate$lastLinesOfRecords <- grep("^SO",sourceLines);
    res$intermediate$exportSource <- 'medline';

    if (!silent) {
      cat0("\nZero hits: looked for PubMed RIS export ('medline') markers:\n");
      cat0("Extracted ", length(res$intermediate$firstLinesOfRecords),
           " lines matching regex '^PMID' (PubMed RIS start of record).\n");
      #cat0("Extracted ", length(res$intermediate$lastLinesOfRecords), " lines matching regex '^SO' (PubMed RIS end of record).\n");
    }

    if (length(res$intermediate$firstLinesOfRecords) == 0) {
      ### Still nothing? Abort.
      stop("The specified textfile ('", filename, "') does not seem to ",
           "include any records. Records are identified by lines starting ",
           "with the opening/closing tags 'TY'/'ER' (RIS format) or ",
           "'PMID'/'SO' (MEDLINE format, for pubmed).");
    }
  }

  res$intermediate$lastLinesOfRecords <-
    c(res$intermediate$firstLinesOfRecords[2:length(res$intermediate$firstLinesOfRecords)] - 2,
      length(sourceLines));

  if (!silent) cat0("\nExtracting references...\n\n");

  res$intermediate$records <-
    llply(1:min(length(res$intermediate$firstLinesOfRecords),
                length(res$intermediate$lastLinesOfRecords)), function(index) {
    res <- makeReference(
      sourceLines[
        res$intermediate$firstLinesOfRecords[index]:res$intermediate$lastLinesOfRecords[index]
        ]);
    return(res);
  }, .progress=ifelse(interactive(), "text", "none"));
  ### Set correct class
  class(res$intermediate$records) <- "sysrev reference list";

  ### Interpret RIS or MEDLINE fields
  res$intermediate$interpretedRecords <- interpretRISlike(res$intermediate$records,
                                                          exportSource=res$intermediate$exportSource);

  if (!silent) cat0("\nConverting references to dataframe...\n\n");

  res$output$records <-
    listsToDataframe(res$intermediate$interpretedRecords$output$recordList,
                     identifierColumnPattern = NULL, progress=TRUE);

  class(res) <- 'sysrev reference list';
  return(res);

}

makeReference <- function(risLines) {
  ### Take a set of RIS lines that together form a record, then
  ### split the field names and the contents, combine the lines
  ### that belong in one field, and return the result this in a
  ### list
  res <- list();

  ### -- Note - we can no longer do this, if we also want to
  ###    be able to read the pubmed 'medline' exports!
  #   ### Split field tags and content on each line
  #   fieldContentDyads <- strsplit(risLines, "  - ");
  #   ### Create a vector with the field tags
  #   fieldsVector <- unlist(lapply(fieldContentDyads, function(x) {
  #     return(x[1]);
  #   }));
  #   ### Create a vector with the field contents
  #   valuesVector <- unlist(lapply(fieldContentDyads, function(x) {
  #     return(x[2]);
  #   }));

  fieldsVector <- c();
  valuesVector <- c();
  fieldNumber <- 0;

  for (currentLine in 1:length(risLines)) {
    ### Check whether we're continuing the last field contents
    if (grepl("^      (.+)", risLines[currentLine])) {
      ### Still reading a field we started reading earlier
      valuesVector[fieldNumber] <- paste(valuesVector[fieldNumber],
                                         trim(risLines[currentLine]));
    }
    ### Check whether we're reading a new line
    else if (grepl("^(....)- .*", risLines[currentLine])) {
      fieldNumber <- fieldNumber + 1;
      fieldsVector[fieldNumber] <- trim(sub("^(....)- .*", "\\1", risLines[currentLine]));
      valuesVector[fieldNumber] <- trim(sub("^....- (.*)", "\\1", risLines[currentLine]));
    }
    ### Else do nothing!
  }

  ### Generate a vector with all the fields we have
  presentFields <- unique(fieldsVector);

  ### Loop through each field and collapse the values
  ### we have for that field
  for (currentField in presentFields) {
    ### Make a new vector with all values of this field type
    subVector <- valuesVector[fieldsVector == currentField];
    ### Collapse all values in this vector; we collapse using
    ### comma's, unless the field is an author field, then w\e
    ### collapse with " and "
    if (grepl("^A(U|\\d])", currentField)) {
      presentValues <- paste(subVector, collapse=" and ");
    }
    else {
      presentValues <- paste(subVector, collapse=", ");
    }
    res[[currentField]] <- presentValues;
  }

  ### Maybe pass result on and create bibentry objects?

  return(res);
}

