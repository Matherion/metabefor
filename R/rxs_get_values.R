rxs_get_values <- function(studyTree,
                           valueName,
                           entityName = NULL) {

  if (is.null(entity)) {
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

}
