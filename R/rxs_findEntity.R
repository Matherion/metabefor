rxs_findEntity <- function(node, id, value, returnEntity=FALSE) {
  res <-
    Traverse(node$root,
             filterFun=function(nd) {
               if (is.null(nd$value)) {
                 return(FALSE);
               } else if (is.list(nd$value)) {
                 if (id %in% names(nd$value)) {
                   if (is.null(nd$value[[id]]) ||
                       is.na(nd$value[[id]])) {
                     return(FALSE);
                   } else if (nd$value[[id]] == value) {
                     return(TRUE);
                   } else {
                     return(FALSE);
                   }
                 } else {
                   return(FALSE);
                 }
               } else {
                 if (nd$name == id) {
                   if (nf$value == value) return(TRUE);
                 } else {
                   return(FALSE);
                 }
               }
             });
  if (returnEntity) {
    return(res);
  } else {
    return(length(res) > 0);
  }
}
