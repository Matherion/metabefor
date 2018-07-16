rxs_extractedListEntities <- function(rxs,
                                      valueRegex = ".*",
                                      entityName = NULL,
                                      withinEntity = NULL) {


  if ("rxs_parsedExtractionScripts" %IN% class(rxs)) {
    ### Processing multiple studies

    res <- lapply(names(rxs$rxsTrees), function(studyName) {
      ### data.tree seems to strip the first class for some reason;
      ### so we use structure to manually force it.
      values <- rxs_extractedListEntities(rxs=structure(rxs$rxsTrees[[studyName]],
                                                        class=c("rxs", "Node", "R6")),
                                          valueRegex=valueRegex,
                                          entityName=entityName,
                                          withinEntity=withinEntity);
      if (is.null(values)) {
        return(NULL);
      } else {
        values <- cbind(rep(studyName, nrow(values)),
                        values);
        names(values) <- c("study", "path", "entity");
        return(values);
      }
    });

    res <- do.call(rbind, res);

    res$study <- sub(pattern='(\\.rxs\\.Rmd)?.?[0-9]*$',
                     replacement="",
                     x=res$study,
                     ignore.case=TRUE);

    return(res);

  } else if ("rxs" %IN% class(rxs)) {
    ### Processing one study

    if (is.null(entityName) && is.null(withinEntity)) {
      filterFun = NULL;
    } else if (!is.null(entityName) && is.null(withinEntity)) {
      filterFun = function(node) {
        return(node$name == entityName);
      }
    } else if (is.null(entityName) && !is.null(withinEntity)) {
      filterFun = function(node) {
        return(withinEntity %IN% node$path);
      }
    } else {
      filterFun = function(node) {
        return((node$name == entityName) &&
                 (withinEntity %IN% node$path));
      }
    }

    res <- rxs$Get(function(node) {
      nodeName <- node$name;
      nodeValue <- node$value;
      if (is.null(nodeValue)) {
        nodeValue <- "";
        names(nodeValue) <- "";
      }
      if (is.list(nodeValue)) {
        pathString <- node$pathString;
        tryCatch(res <- data.frame(path = rep(pathString, length(nodeValue)),
                                   entity = names(nodeValue),
                                   wasExtracted = sapply(nodeValue, function(x) return(!all(is.na(x)))),
                                   stringsAsFactors = FALSE),
                 error = function(e) {
                   print(nodeValue);
                   cat0(pathString, "\n\n",
                        class(nodeValue));
                   return(NULL);
                 });
        return(res);
      } else {
        if (node$isRoot) {
          pathString <- node$pathString;
        } else {
          pathString <- node$parent$pathString;
        }
        return(data.frame(path = pathString,
                          entity = nodeName,
                          wasExtracted = !all(is.na(nodeValue)),
                          stringsAsFactors = FALSE));
      }
    }, filterFun = filterFun,
    simplify=FALSE);

    if (is.null(res)) {
      return(res);
    } else {
      res <- do.call("rbind", res);
      res <- res[res$wasExtracted, c('path', 'entity')];
      res <- res[grepl(valueRegex, res$entity), ];
      row.names(res) <- NULL;
      return(res);
    }
  }
}
