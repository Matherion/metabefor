rxs_fg_valueTemplateValidation <- function(node,
                                           valueTemplates,
                                           level = 0,
                                           indent = TRUE,
                                           indentSpaces = 2,
                                           fullWidth = 80,
                                           commentCharacter = "#",
                                           fillerCharacter = "#",
                                           eC = entityColNames()) {

  ### This function looks up (or generates) the examples for
  ### an extractable entity.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argment 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  valueTemplate <- valueTemplates[[node[[eC$valueTemplateCol]]]];

  if (is.na(valueTemplate$validation) || (nchar(valueTemplate$validation) == 0)) {
    return(NULL);
  } else {
    res <- valueTemplate$validation;
  }

  ### Possibly override with value from entity specification
  if (!is.null(node$validation) && !is.na(node$validation) && !(nchar(node$validation) == 0)) {
    res <- node$validation;
  }

  ### Do fieldname replacement using regular expressions, if need be
  allEntityFieldNames <- paste0("<<", eC, ">>");
  fieldNameReplacementHits <- sapply(allEntityFieldNames, grepl, x=res);
  if (any(fieldNameReplacementHits)) {
    for (i in which(fieldNameReplacementHits)) {
      fieldNameReplacementContents <-
        node[[eC[[i]]]];
      if (grepl("\\|\\|", fieldNameReplacementContents)) {
        ### In this case, it's a list, so change it into a
        ### valid vector
        fieldNameReplacementContents <-
          paste0("c(",
                 paste0(trim(unlist(strsplit(fieldNameReplacementContents,
                                             "||", fixed=TRUE))),
                             collapse=", "),
                 ")");
      }

      res <- gsub(allEntityFieldNames[i],
                  fieldNameReplacementContents,
                  res);
    }
  }

  return(res);

}
