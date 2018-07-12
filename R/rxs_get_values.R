rxs_get_values <- function(rxs,
                           valueName,
                           entityName = NULL,
                           withinEntity = NULL) {

  if ("rxs_parsedExtractionScripts" %IN% class(rxs)) {
    ### Processing multiple studies

    res <- lapply(names(rxs$rxsTrees), function(studyName) {
      ### data.tree seems to strip the first class for some reason;
      ### so we use structure to manually force it.
      values <- rxs_get_values(rxs=structure(rxs$rxsTrees[[studyName]],
                                             class=c("rxs", "Node", "R6")),
                               valueName=valueName,
                               entityName=entityName,
                               withinEntity=withinEntity);
      if (is.null(values)) {
        return(NULL);
      } else {
        values <- list(values);
        names(values) <- studyName;
        return(values);
      }
    });

    res <- unlist(res);

    if (!is.null(res)) {
      valueNames <- names(res);
      valueNames <- sub(pattern=paste0('(\\.rxs\\.Rmd)?.?[0-9]*$'),
                        replacement="",
                        x=valueNames,
                        ignore.case=TRUE);
      names(res) <- valueNames;
    }

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

    values <- rxs$Get(function(node) {
      if (!is.null(node[[valueName]]$value)) {
        ### Value is stored in this node as single value
        return(node[[valueName]]$value);
      } else if (is.null(node$value)) {
        return(NULL);
      } else if (is.list(node$value) && (valueName %IN% names(node$value))) {
        return(node$value[[valueName]]);
      } else if (!is.null(node[[valueName]])) {
        return(node[[valueName]]);
      } else {
        return(NULL);
      }
    }, filterFun = filterFun);

    values <- unlist(values);
    values <- values[!is.na(values)];

    if (length(values) == 0) {
      return(NULL);
    } else {
      names(values) <- NULL;
      return(values);
    }
  } else {
    stop("Class of object provided as argument 'rxs' must be or contain 'rxs_parsedExtractionScripts' ",
         "or 'rxs' (currently ", vecTxtQ(class(rxs)), ").");
  }

}
