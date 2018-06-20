print.rxs <- function(studyTree,
                      rxsStructure=NULL,
                      ...) {
  res <- studyTree$Get(function(node) {
    nodeName <- node$name;
    nodeValue <- node$value;
    if (is.null(nodeValue)) {
      nodeValue <- "";
    }
    if (is.list(nodeValue)) {
      pathString <- node$pathString;
      return(data.frame(path = rep(pathString, length(nodeValue)),
                        entity = names(nodeValue),
                        nodeValue = unlist(nodeValue)));
    } else {
      pathString <- node$parent$pathString;
      return(data.frame(path = pathString,
                        entity = nodeName,
                        nodeValue = nodeValue));
    }
  }, filterFun = isLeaf,
  simplify=FALSE);

  res <- do.call("rbind", res)

  cat("\n\n# Tree of extracted entities\n\n");

  printableStudyTree <- Clone(studyTree);
  class(printableStudyTree) <- setdiff(class(study), "rxs");

  print(printableStudyTree);

  cat("\n\n# Table with extracted entities and extracted values\n\n");

  return(res);
}
