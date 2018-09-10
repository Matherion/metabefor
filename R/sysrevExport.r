sysrevExport <- function (libraryObject, filename, screeningType=NULL,
                          format="bibtex", drop=NULL, keep=NULL,
                          sep="\t", encoding = "UTF-8",
                          row.names = FALSE, ...) {

  ### Check whether we have a dataframe; copy it if it's in the wrong
  ### position
  if (is.null(libraryObject$output$records)) {
    if (is.null(is.data.frame(libraryObject$records))) {
      stop("sysrevExport requires a dataframe with bibliographic entries that is ",
           "stored in $records or in $output$records, but this is not present!");
    }
    else {
      if (!is.null(libraryObject$records)) {
        if (is.data.frame(libraryObject$records)) {
          libraryObject$output$records <- libraryObject$records;
        }
      }
      else {
        stop("sysrevExport requires a dataframe with bibliographic entries that is ",
             "stored in $records or in $output$records, but the object stored in $records ",
             "is not a dataframe!");
      }
    }
  }
  else {
    if (!is.data.frame(libraryObject$output$records)) {
      stop("sysrevExport requires a dataframe with bibliographic entries that is ",
           "stored in $records or in $output$records, but the object stored in $output$records ",
           "is not a dataframe!");
    }
  }

  ### Check other arguments
  if (FALSE %in% (drop %in% names(libraryObject$output$records))) {
    warning("The following columns (fields) to drop do not exist in the reference library: ",
            paste(drop[!(drop %in% names(libraryObject$output$records))],
                  collapse=", "));
  }
  if (FALSE %in% (keep %in% names(libraryObject$output$records))) {
    warning("The following columns (fields) to keep do not exist in the reference library: ",
            paste(keep[!(keep %in% names(libraryObject$output$records))],
                  collapse=", "));
  }
  if (!is.null(drop) & !(is.null(keep))) {
    stop("Cannot both keep and drop columns (fields)! When you specify columns (fields) to ",
         "drop, all other fields will be kept; when you specify columns (fields) to keep, ",
         "all other fields will be dropped.");
  }

  ### Drop columns if needed
  if(!is.null(drop)) {
    ### Dropping columns (fields) to drop
    dataframeToWrite <- libraryObject$output$records[ , setdiff(names(libraryObject$output$records), drop)];
  }
  else if(!is.null(keep)) {
    ### Write records to file, dropping columns (fields) to drop
    dataframeToWrite <- libraryObject$output$records[ , keep];
  }
  else {
    ### Write everything
    dataframeToWrite <- libraryObject$output$records;
  }

  ### Write records to file
  if (format == "table") {
    ### Write as tab separated file (probably)
    write.table(dataframeToWrite,
                filename, sep = sep,
                row.names = row.names,
                fileEncoding = encoding, ...);
  }
  else {
    ### Write in bibtex format
    writable <- dataframeToBibTex(dataframeToWrite, screeningType=screeningType);

    ### Replace non-ASCII characters
    #writable <- iconv(writable, "UTF-8", "ASCII//TRANSLIT");
    ### (obsolete - writing output at UTF-8)

    ### Write
    con <- file(filename, "w", encoding = encoding);
    writeLines(writable, con);
    close(con);
  }
}

