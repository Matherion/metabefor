isRecurringEntityInclusion <- function(node,
                                       recurringColName=entityColNames()$recurringCol) {
  return((!is.null(node[[recurringColName]])) &&
         (!is.na(node[[recurringColName]])) &&
         (is.character(node[[recurringColName]])) &&
         (!(toupper(node[[recurringColName]])=="TRUE")));
}
