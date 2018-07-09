rxs_parseExtractionScripts <- function(path,
                                       pattern="\\.rxs\\.Rmd",
                                       exclude="extractionScriptTemplate.rxs.Rmd",
                                       ignore.case=TRUE) {

  res <- list(input = as.list(environment()));

  allScripts <- list.files(path,
                           pattern=pattern,
                           ignore.case=ignore.case);
  allScripts <- setdiff(allScripts,
                        exclude);

  res$rxsOutput <- list();
  res$rxsTrees <- list();

  for (filename in allScripts) {
    ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document
    tempR <- tempfile(tmpdir = ".", fileext = ".R");
    on.exit(unlink(tempR));
    knitr::purl(rmd, output=tempR);
    res$rxsOutput[[filename]] <-
      capture.output(tryCatch(sys.source(tempR, envir=globalenv())),
                             error = function(e) {
                               cat0("\n\nEncountered error: \n\n");
                               print(e);
                               invisible(e);
                             });
    if (exists(study, envir=globalenv())) {
      res$rxsTrees[[filename]] <- get('study', envir=globalenv());
      rm(study, envir=globalenv());
    } else {
      res$rxsTrees[[filename]] <- NA;
    }
  }

  return(res);

}
