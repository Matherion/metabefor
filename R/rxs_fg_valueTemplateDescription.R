rxs_fg_valueTemplateDescription <- function(node,
                                            valueTemplates,
                                            level = 0,
                                            indent = TRUE,
                                            indentSpaces = 2,
                                            fullWidth = 80,
                                            commentCharacter = "#",
                                            fillerCharacter = "#",
                                            eC = entityColNames(),
                                            listVersion = FALSE) {

  ### This function look up (or generates) the default value for
  ### an extractable entity.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argument 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  if (isTRUE(nchar(node[[eC$recursingCol]]) > 0) || isTRUE(nchar(node[[eC$recurringCol]]) > 0)) {
    return("");
  }

  if (is.na(node[[eC$valueTemplateCol]]) ||
    is.null(node[[eC$valueTemplateCol]]) ||
    nchar(node[[eC$valueTemplateCol]]) == 0) {
    stop("In the entity specifications, the value template for '",
         node$name, "' is not specified!");
  } else if (!(node[[eC$valueTemplateCol]] %in% names(valueTemplates))) {
    stop("The value template specified in the entity specification '",
         node[[eC$valueTemplateCol]],
         "' does not exist in the list of value template specifications!");
  } else {                
    valueTemplate <- valueTemplates[[node[[eC$valueTemplateCol]]]];
  }

  if (is.null(valueTemplate$description) || is.na(valueTemplate$description) || (nchar(valueTemplate$description) == 0)) {
    return(NULL);
  } else {
    res <- valueTemplate$description;
  }

  if (!listVersion) {
    lV <- rxs_fg_layoutVars(level = level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);
    res <- strwrap(res,
                   width=lV$commentWidth,
                   prefix=lV$commentPrefix);
  }

  return(res);

}
