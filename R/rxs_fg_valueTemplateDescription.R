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
    stop("Argment 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  valueTemplate <- valueTemplates[[node[[eC$valueTemplateCol]]]];

  if (is.na(valueTemplate$description) || (nchar(valueTemplate$description) == 0)) {
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
