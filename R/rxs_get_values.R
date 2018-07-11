rxs_get_values <- function(rxs,
                           valueName,
                           entityName = NULL) {

  if ("rxs_parsedExtractionScripts" %IN% class(rxs)) {
    ### Processing multiple studies

    res <- unlist(lapply(names(rxs$rxsTrees), function(studyName) {
      values <- rxs_get_values(rxs$rxsTrees[[studyName]],
                               valueName,
                               entityName);
      values <- list(opers[!is.na(opers)]);
      names(values) <- studyName;
      return(values);
    }));

    return(res);

  } else if ("rxs" %IN% class(rxs)) {
    ### Processing one study

    if (is.null(entityName)) {
      filterFun = NULL;
    } else {
      filterFun = function(node) {
        return(node$name == entityName);
      }
    }

    values <- studyTree$Get(function(node) {
      if (!is.null(node[[valueName]]$value)) {
        ### Value is stored in this node as single value
        return(node[[valueName]]$value);
      } else if (is.null(node$value)) {
        return(NULL);
      } else if (is.list(node$value) && (valueName %IN% names(node$value))) {
        return(node$value[[valueName]]);
      } else {
        return(NULL);
      }
    }, filterFun = filterFun);

    return(unlist(values));

  } else {
    stop("Class of object provided as argument 'rxs' must be or contain 'rxs_parsedExtractionScripts' ",
         "or 'rxs' (currently ", vecTxtQ(rxs), ").");
  }

}
