isRecursingEntityInclusion <- function(node,
                                       recursingColName=entityColNames()$recursingCol) {
  return((!is.null(node[[recursingColName]])) &&
         (!is.na(node[[recursingColName]])) &&
         (is.character(node[[recursingColName]])) &&
         (!(toupper(node[[recursingColName]])=="TRUE")));
}
