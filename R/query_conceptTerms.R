query_conceptTerms <- function(...,
                               conceptName = NULL,
                               operator="OR") {
  res <- unlist(list(...));
  attr(res, "conceptName") <- conceptName;
  attr(res, "operator") <- operator;
  class(res) <- 'query_conceptTerms';
  return(res);
}

print.query_conceptTerms <- function(x, ...) {
  if (!is.null(attr(x, "conceptName"))) {
    cat0("List of terms for concept \"",
         attr(x, "conceptName"),
         "\", to be combined with the ",
         attr(x, "operator"),
         " operator:\n");
  } else {
    cat0("List of terms for a concept in a systematic review, to be combined with the ",
         attr(x, "operator"),
         " operator:\n");
  }
  cat0("\n",
       paste0("- ", x, "\n"),
       "\n");
  invisible(x);
}
