importBibtex <- function(filename, encoding="UTF-8",
                         lowercaseColNames = TRUE,
                         silent = FALSE) {
  ### Read lines from bibtex file
  con <- file(filename, encoding=encoding);
  sourceLines <- readLines(con = con, encoding = encoding);
  close(con)

  ### Set up variables we'll need
  pos <- 1;
  nrOfRecords <- 0;
  status.readingPreamble <- FALSE;
  status.readingRecord <- FALSE;
  status.readingMeta <- FALSE;
  status.currentMeta <- '';
  status.currentField <- '';
  res <- list(filename = filename,
              preamble='',
              records = data.frame(),
              comments = "",
              jabrefmeta = list(groupversion = NULL,
                                entrytypes = list(),
                                misc = list()));

  ### Loop through each line in the file
  while(pos <= length(sourceLines)) {
    ### Check whether this line's a comment; if so, just add it to the
    ### comments and move on.
    if (substr(sourceLines[pos], 1, 1) == "%") {
      res$comments <- paste0(res$comments, "\n", sourceLines[pos]);
    }
    ### Check whether we're reading the preable
    else if (status.readingPreamble) {
      ### Check whether we should finish reading
      ### the preamble
      if (grepl("}", sourceLines[pos], fixed=TRUE)) {
        ### There is a closing brace, so just store the line
        ### up until the closing brace
        res$preamble <-
          paste0(res$preamble,
                 substr(sourceLines[pos], 1, regexpr("}", sourceLines[pos], fixed=TRUE)[1] - 1));
        ### And stop reading the preamble on the next lines
        status.readingPreamble <- FALSE;
      }
      ### No closing brace on this line, so just add the line to the preamble
      else {
        res$preamble <- paste0(res$preamble, sourceLines[pos]);
      }
    }
    else if (status.readingMeta) {
      ### Check whether we have to stop reading this JabRef meta field
      if (grepl("}", sourceLines[pos], fixed=TRUE)) {
        ### Stop reading this meta field
        status.readingMeta <- FALSE;
        status.currentMeta <- "";
      }
      else {
        res$jabrefmeta[[status.currentMeta]] <- paste0(res$jabrefmeta[[status.currentMeta]], '\n',
                                                       sourceLines[pos]);
      }
    }
    ### We're not reading a preamble or meta comment, so check
    ### whether we're reading a record
    else if (!status.readingRecord) {
      ### We're not reading a record, so
      ### check whether we should start
      ### reading a preamble or a record
      if (grepl("@PREAMBLE{", toupper(sourceLines[pos]), fixed=TRUE)) {
        ### Check whether the preamble is only one line long
        if (grepl("}", sourceLines[pos], fixed=TRUE)) {
          ### There is a closing brace, so just store the line
          ### up until the closing brace and we're done
          res$preamble <- substr(sourceLines[pos], 1, regexpr("}", sourceLines[pos], fixed=TRUE)[1] - 1);
        }
        else {
          ### No closing brace, so keep on reading
          ### the preamble on the next line
          status.readingPreamble <- TRUE;
          ### And store this line in the preamble
          res$preamble <- sourceLines[pos];
        }
      }
      else if (grepl("@comment\\{jabref-meta: (.*)", tolower(sourceLines[pos]))) {
        ### A JabRef comment; figure out what kind.
        if (grepl("@comment\\{jabref-meta: groupsversion:(.*);\\}", tolower(sourceLines[pos]))) {
          res$jabrefmeta$groupsversion <- sub("@comment\\{jabref-meta: groupsversion:(.*);\\}", '\\1', tolower(sourceLines[pos]));
        }
        else if (grepl("@comment\\{jabref-meta: (.*):(.*)", tolower(sourceLines[pos]))) {
          ### Toggle reading of jabref meta comment
           status.readingMeta <- TRUE;
           ### Extract name of meta comment
           status.currentMeta <- sub("@comment\\{jabref-meta: (.*):(.*)", "\\1", tolower(sourceLines[pos]));
           ### store any content on this first line (probably nothing) in a list
           res$jabrefmeta[[status.currentMeta]] <- sub("@comment\\{jabref-meta: (.*):(.*)", "\\2", tolower(sourceLines[pos]));
        }
      }
      else if (grepl("@comment\\{jabref-entrytype: (.*)\\}", tolower(sourceLines[pos]))) {
        ### Add this entry type to the result object
        res$jabrefmeta$entrytypes[[length(res$jabrefmeta$entrytypes) + 1]] <-
          sub("@comment\\{jabref-entrytype: (.*)\\}", '\\1', tolower(sourceLines[pos]));
      }
      else if (grepl("@[a-zA-Z0-9]+\\{", sourceLines[pos])) {
        ### We have to start reading a record. Start by
        ### storing the record type
        nrOfRecords <- nrOfRecords + 1;
        res$records[nrOfRecords, 'recordType'] <-
          trim(sub("@([a-zA-Z0-9]+)\\{.*", "\\1", sourceLines[pos]));
        ### Check for a bibtexkey and store it if we have one
        if (grepl("@[a-zA-Z0-9]+\\{.+,", sourceLines[pos])) {
          res$records[nrOfRecords, 'bibtexkey'] <-
            trim(gsub("@([a-zA-Z0-9]+)\\{(.*),", "\\2", sourceLines[pos]));
        }
        ### Store status so we'll keep on reading the record
        ### on the next lines
        status.readingRecord <- TRUE;
      }
      ### If this didn't match, either, this line doesn't start a preamble or
      ### new record, so ignore it.
    }
    ### We're reading a record
    else {
      ### Check whether we're finishing a record
      if (sourceLines[pos] == "}") {
        status.readingRecord <- FALSE;
        status.currentField <- '';
      }
      else {
        ### First check whether this line contains a field name and the complete contents
        if (grepl("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)}\\s*,?", sourceLines[pos])) {
          status.currentField <- trim(gsub("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)}\\s*,?", "\\1", sourceLines[pos]));
          res$records[nrOfRecords, status.currentField] <-
            trim(gsub("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)}\\s*,?", "\\2", sourceLines[pos]));
        }
        ### Annoyingly enough, JabRef doesn't use curly braces around months
        else if (grepl("month = (.+),", sourceLines[pos])) {
          res$records[nrOfRecords, 'month'] <-
            trim(gsub("month = (.+),", "\\1", sourceLines[pos]));
        }
        ### If not, check whether this line contains a
        ### field name and the start of the contents
        else if (grepl("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)", sourceLines[pos])) {
          status.currentField <- trim(gsub("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)", "\\1", sourceLines[pos]));
          res$records[nrOfRecords, status.currentField] <-
            trim(gsub("([A-Za-z0-9_-]+)\\s*=\\s*\\{(.+)", "\\2", sourceLines[pos]));
        }
        ### If not, check whether this line contains a field name and empty contents
        else if (grepl("([A-Za-z0-9_-]+)\\s*=\\s*\\{}\\s*,?", sourceLines[pos])) {
          status.currentField <- trim(gsub("([A-Za-z0-9_-]+)\\s*=\\s*\\{}\\s*,?", "\\1", sourceLines[pos]));
          res$records[nrOfRecords, status.currentField] <-
            "";
        }
        ### If not, check whether this line contains additional
        ### contents for whichever field we're reading now,
        ### and ends the reading of this field (i.e. closes it)
        else if (grepl("(.*)},?", sourceLines[pos])) {
          res$records[nrOfRecords, status.currentField] <-
            paste(res$records[nrOfRecords, status.currentField],
                  trim(gsub("(.*)},?", "\\1", sourceLines[pos])));
          status.currentField <- '';
        }
        ### This line just contains additional contents
        ### for whichever field we're reading now
        else {
          tryCatch({
            res$records[nrOfRecords, status.currentField] <-
              paste(res$records[nrOfRecords, status.currentField],
                    trim(sourceLines[pos]))
            }, error=function(errorMsg) {
              stop("importBibtex encountered an error while reading file '", filename,
                   "' on line ", pos, ":\n",
                   sourceLines[pos], "\n\n",
                   "status.readingPreamble: ", status.readingPreamble, "\n",
                   "status.readingRecord: ", status.readingRecord, "\n",
                   "status.readingMeta: ", status.readingMeta, "\n",
                   "\nR error message: ", errorMsg);
            });
        }
      }
    }
    pos <- pos + 1;
  }

  if (lowercaseColNames) {
    names(res$records) <- tolower(names(res$records));
  }

  if (!silent) cat0("\nConverting references to dataframe...\n\n");

  res$output$records <- res$records;

  class(res) <- 'sysrev reference object';
  return(res);

}
