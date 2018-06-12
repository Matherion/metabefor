rxs_fg_valueTemplateExamples <- function(node,
                                         valueTemplates,
                                         level = 0,
                                         indent = TRUE,
                                         indentSpaces = 2,
                                         fullWidth = 80,
                                         commentCharacter = "#",
                                         fillerCharacter = "#",
                                         eC = entityColNames(),
                                         listVersion = FALSE) {

  ### This function looks up (or generates) the examples for
  ### an extractable entity.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argment 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  valueTemplate <- valueTemplates[[node[[eC$valueTemplateCol]]]];

  if (is.na(valueTemplate$examples) || (nchar(valueTemplate$examples) == 0)) {
    return(NULL);
  } else {
    res <- valueTemplate$examples;
  }

  ### Possibly override with value from entity specification
  if (!is.null(node$examples) && !is.na(node$examples) && !(nchar(node$examples) == 0)) {
    res <- node$examples;
  }

  if (listVersion) {
    res <- trim(unlist(strsplit(res, "||", fixed=TRUE)));
    return(res);
  } else {
    lV <- rxs_fg_layoutVars(level = level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);
    res <- paste0(lV$commentPrefix, trim(unlist(strsplit(res, "||", fixed=TRUE))));
  }

  if (!listVersion) {
    if (length(res) > 1) {
      examplesTxt <- paste0(lV$commentPrefix, "EXAMPLES:");
    } else {
      examplesTxt <- paste0(lV$commentPrefix, "EXAMPLE:");
    }
    res <- c(examplesTxt,
             lV$commentPrefix,
             res);
  }

  return(res);

}
