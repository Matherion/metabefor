print.rxs <- function(studyTree,
                      rxsStructure=NULL,
                      knit=TRUE,
                      headerPrefix = "### ",
                      ...) {

  flattenNodeValues <- function(x) {
    res <- lapply(x, function(singleValue) {
      if (is.null(singleValue)) {
        return(NULL);
      } else if (all(is.na(singleValue))) {
        return(NA);
      } else if (length(singleValue) == 1) {
        return(singleValue);
      } else if (is.vector(singleValue)) {
        return(ufs::vecTxtQ(singleValue));
      } else if (is.matrix(singleValue)) {
        return(paste(apply(singleValue, 1, vecTxtQ), collapse="\n"));
      } else {
        return(capture.output(str(singleValue)));
      }
    });
    return(unlist(res));
  }

  res <- studyTree$Get(function(node) {
    nodeName <- node$name;
    nodeValue <- node$value;
    if (is.null(nodeValue)) {
      nodeValue <- "";
    }
    if (is.list(nodeValue)) {
      pathString <- node$pathString;

      ### Note - this will become problematic if the list contains
      ### more complicated values such as vectors or tables!!!

      return(data.frame(path = rep(pathString, length(nodeValue)),
                        entity = names(nodeValue),
                        nodeValue = flattenNodeValues(nodeValue),
                        stringsAsFactors = FALSE));
    } else {
      pathString <- node$parent$pathString;
      return(data.frame(path = pathString,
                        entity = nodeName,
                        nodeValue = flattenNodeValues(nodeValue),
                        stringsAsFactors = FALSE));
    }
  }, filterFun = isLeaf,
     simplify=FALSE);

  res <- do.call("rbind", res)

  cat0(headerPrefix, "\n\nTree of extracted entities\n\n");

  printableStudyTree <- Clone(studyTree);
  class(printableStudyTree) <- setdiff(class(study), "rxs");

  if (knit) cat("\n\n<pre>");
  ### Suppress warnings until bug in data.tree is fixed, see:
  ### https://github.com/gluc/data.tree/issues/106
  suppressWarnings(print(printableStudyTree));
  if (knit) cat("</pre>\n\n");

  cat0(headerPrefix, "\n\nTable with extracted entities and extracted values\n\n");

  if (knit) {
    cat(knitr::knit(text = "\n\n```{r extracted-data-chunk, echo=FALSE, cache=FALSE, message=FALSE, results='markup' }\n  pander(res, row.names=FALSE);\n```\n\n",
                    quiet = TRUE));
    invisible(res);
  } else {
    return(res);
  }

}
