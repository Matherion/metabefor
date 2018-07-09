rxs_parseExtractionScripts <- function(path,
                                       pattern="\\.rxs\\.Rmd",
                                       exclude="extractionScriptTemplate.rxs.Rmd",
                                       ignore.case=TRUE) {

  res <- list(input = as.list(environment()));

  allScripts <- list.files(path,
                           pattern=pattern,
                           ignore.case=ignore.case);

  for (exclusionPattern in exclude) {
    allScripts <- grep(exclusionPattern,
                       allScripts,
                       value=TRUE,
                       invert=FALSE);
  }

  res$rxsOutput <- list();
  res$rxsTrees <- list();

  for (filename in allScripts) {
    ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document

    ### Create temporary file
    tempR <- tempfile(tmpdir = ".", fileext = ".R");

    ### Make sure it's deleted when we're done
    on.exit(unlink(tempR));

    ### Extract R chunks and write them to another file
    knitr::purl(file.path(path,
                          filename),
                output=tempR,

                quiet=TRUE);

    ### Run the other file with error handling
    res$rxsOutput[[filename]] <-
      capture.output(tryCatch(sys.source(tempR, envir=globalenv())),
                             error = function(e) {
                               cat0("\n\nEncountered error: \n\n");
                               print(e);
                               invisible(e);
                             });

    ### If successfull, store the result and delete object; otherwise set to NA
    if (exists('study', envir=globalenv())) {
      res$rxsTrees[[filename]] <-
        data.tree::Clone(get('study', envir=globalenv()));
      rm(study, envir=globalenv());
    } else {
      res$rxsTrees[[filename]] <- NA;
    }
  }

  return(res);

}
