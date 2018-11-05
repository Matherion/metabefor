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

  for (filename in allScripts) {
    ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document

    ### Create temporary file
    tempR <- tempfile(fileext = ".R");

    ### Make sure it's deleted when we're done
    on.exit(unlink(tempR));

    ### Extract R chunks and write them to another file
    res$rxsPurlingOutput[[filename]] <-
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
      res$rxsOutput[[filename]] <-
        capture.output(tryCatch(sys.source(tempR, envir=parent.frame()),
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
    if (exists('study', envir=parent.frame())) {
      res$rxsTrees[[filename]] <-
        data.tree::Clone(get('study', envir=parent.frame()));
      rm(study, envir=parent.frame());
    } else {
      res$rxsTrees[[filename]] <- NA;
    }
  }

  class(res) <- "rxs_parsedExtractionScripts";

  return(res);

}
