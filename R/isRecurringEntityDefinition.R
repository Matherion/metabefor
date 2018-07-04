isRecurringEntityDefinition <- function(node,
                                        recurringColName=entityColNames()$recurringCol) {
  return(((!is.null(node[[recurringColName]])) &&
          (!is.na(node[[recurringColName]])) &&
            (isTRUE(node[[recurringColName]]) ||
            (toupper(node[[recurringColName]])=="TRUE"))));
}
