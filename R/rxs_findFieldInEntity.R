rxs_findFieldInEntity <- function(node, id, value) {
  if (node$value$
  entity <- rxs_findEntity(
  return(length(Traverse(node$root,
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
                         })) > 0);
}
