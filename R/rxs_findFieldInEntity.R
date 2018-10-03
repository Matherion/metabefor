rxs_findFieldInEntity <- function(node, id, value) {
  if (is.null(node$entityRefs)) {
    return(FALSE);
  } else if (is.null(node$value)) {
    return(FALSE);
  } else if (!any(names(node$entityRefs) %in% names(node$value))) {
    return(FALSE);
  } else {
    ### Get the entities that are referred to in this entity list
    referredToNodeNames <-
      node$value[names(node$entityRefs)[which(names(node$entityRefs) %in% names(node$value))]];
    ### Get the contents of the field names 'id' in the nodes
    ### that are referred to in this entity list
    fieldContents <-
      node$root$Get(id,
                    function(nd) {
                      return(nd$name %in% referredToNodeNames);
                    });
    fieldContents <- fieldContents[!is.na(fieldContents)];
    if (length(fieldContents) > 0) {
      return(TRUE);
    } else {
      return(FALSE);
    }
  }
}
