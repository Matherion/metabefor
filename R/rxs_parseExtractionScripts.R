rxs_parseExtractionScripts <- function(path,
                                       pattern="\\.rxs\\.Rmd",
                                       exclude=c("extractionScriptTemplate.rxs.Rmd",
                                                 "\\[EXCLUDED]"),
                                       ignore.case=TRUE,
                                       recursive=TRUE,
                                       quiet=TRUE,
                                       showErrors=TRUE,
                                       encoding="UTF-8") {

  res <- list(input = as.list(environment()));

  allScripts <- list.files(path,
                           pattern=pattern,
                           ignore.case=ignore.case,
                           recursive=recursive);

  for (exclusionPattern in exclude) {
    allScripts <- grep(exclusionPattern,
                       allScripts,
                       value=TRUE,
                       invert=TRUE);
  }

  res$rxsOutput <- list();
  res$rxsTrees <- list();

  if (anyDuplicated(allScripts)) {
    warning("Warning: two rxs files with the same name found: ",
            vecTxtQ(allScripts[duplicated(allScripts)]),
            ".");
    allScripts <- unique(allScripts);
  }

  res$input$allScripts <- allScripts;

  if (interactive()) {
    p <- dplyr::progress_estimated(length(allScripts));
  };

  for (filename in allScripts) {
    ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document

    ### Create temporary file
    tempR <- tempfile(fileext = ".R");

    ### Make sure it's deleted when we're done
    on.exit(unlink(tempR));

    if (filename %in% names (res$rxsPurlingOutput)) {
      warning("RXS purling output was already stored for file '",
              filename,
              "'. Storing existing version as 'BACKUP-",
              filename,
              "'.");
      res$rxsPurlingOutput[[paste0("BACKUP-",filename)]] <-
        res$rxsPurlingOutput[[filename]];
      res$rxsPurlingOutput[[filename]] <- NULL;
    }

    ### Extract R chunks and write them to another file
    purlingOutput <-
      capture.output(tryCatch(knitr::purl(file.path(path,
                                                    filename),
                                          output=tempR,
                                          quiet=quiet,
                                          encoding=encoding),
                              error = function(e) {
                                cat(paste0("In file '",
                                           filename,
                                           "', encountered error while purling: \n",
                                           e$message,
                                           "\n\n",
                                           collapse="\n"));
                                invisible(e);
                              }));
    tryCatch({res$rxsPurlingOutput[[filename]] <-
                purlingOutput;},
             error = function(e) {
               stop("Error saving purling output to rxs object! The error is:\n\n",
                    e$message,
                    "\n\nEncountered while processing file '", filename, "'.\n");
             });

    if (any(grepl("In file '",
                    filename,
                    "', encountered error while purling",
                    res$rxsPurlingOutput[[filename]]))) {
      res$rxsOutput[[filename]] <-
        "Could not run this extraction script because of purling problems."
      if (showErrors) {
        cat(paste0(res$rxsPurlingOutput[[filename]], collapse="\n"));
      }
    } else {
      ### Run the other file with error handling
      rxsOutput <-
        capture.output(tryCatch(sys.source(tempR, envir=globalenv()),
                                error = function(e) {
                                  cat(paste0("In file '",
                                             filename,
                                             "', encountered error while running rxs: \n",
                                             e$message,
                                             "\n\n",
                                             collapse="\n"));
                                  # cat(e$message);
                                  invisible(e);
                                }));
      tryCatch({res$rxsOutput[[filename]] <-
        rxsOutput;},
        error = function(e) {
          stop("Error saving rxs evaluation output to rxs object! The error is:\n\n",
               e$message,
               "\n\nEncountered while processing file '", filename, "'.\n");
        });

      if (showErrors) {
        if (any(grepl("In file '",
                      filename,
                      "', encountered error while running rxs",
                      res$rxsOutput[[filename]]))) {
          cat(paste0(res$rxsOutput[[filename]], collapse="\n"));
        }
      }
    }

    ### If successful, store the result and delete object; otherwise set to NA
    if (exists('study', envir=globalenv())) {
      res$rxsTrees[[filename]] <-
        data.tree::Clone(get('study', envir=globalenv()));
      rm(study, envir=globalenv());
    } else {
      res$rxsTrees[[filename]] <- NA;
    }

    if (interactive()) {
      p$tick()$print();
    };

  }

  class(res) <- "rxs_parsedExtractionScripts";

  return(res);

}
