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
                                invisible(paste0(c("Encountered error while purling: \n\n",
                                                   e$message,
                                                   "\n"),
                                                 collapse="\n"));
                              }));

    if (showErrors) {
      if (any(grepl("Encountered error while purling",
                    res$rxsPurlingOutput[[filename]]))) {
        cat(paste0(res$rxsPurlingOutput[[filename]], collapse="\n"));
      }
    }

    ### Run the other file with error handling
    res$rxsOutput[[filename]] <-
      capture.output(tryCatch(sys.source(tempR, envir=globalenv()),
                              error = function(e) {
                                invisible(paste0(c("Encountered error while running rxs: \n\n",
                                                   e$message,
                                                   "\n"),
                                                 collapse="\n"));
                                # cat(e$message);
                                # invisible(e);
                              }));

    if (showErrors) {
      if (any(grepl("Encountered error while running rxs",
                    res$rxsOutput[[filename]]))) {
        cat(paste0(res$rxsOutput[[filename]], collapse="\n"));
      }
    }

    ### If successfull, store the result and delete object; otherwise set to NA
    if (exists('study', envir=globalenv())) {
      res$rxsTrees[[filename]] <-
        data.tree::Clone(get('study', envir=globalenv()));
      rm(study, envir=globalenv());
    } else {
      res$rxsTrees[[filename]] <- NA;
    }
  }

  class(res) <- "rxs_parsedExtractionScripts";

  return(res);

}
