rxs_fg_defaultValueAssignment <- function(node,
                                          valueTemplates,
                                          level = 0,
                                          indent = TRUE,
                                          indentSpaces = 2,
                                          fullWidth = 80,
                                          commentCharacter = "#",
                                          fillerCharacter = "#",
                                          eC = entityColNames()) {

  ### This function looks up (or generates) the default value for
  ### an extractable entity.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argment 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  lV <- rxs_fg_layoutVars(level = level,
                          indent = indent,
                          indentSpaces = indentSpaces,
                          fullWidth = fullWidth,
                          commentCharacter = commentCharacter,
                          fillerCharacter = fillerCharacter);

  valueTemplate <- valueTemplates[[node[[eC$valueTemplateCol]]]];

  if (is.na(valueTemplate$default) || (nchar(valueTemplate$default) == 0)) {
    res <- "NA";
  } else {
    res <- valueTemplate$default;
  }

  ### Possibly override with value from entity specification
  if (!is.null(node$default) && !is.na(node$default) && !(nchar(node$default) == 0)) {
    res <- node$default;
  }

  res <- paste0(lV$valuePrefix, res);

  if (grepl("<<<([^<]*)>>>", res)) {
    colWithTextToInclude <-
      sub(".*<<<([^<]*)>>>.*", "\\1", res);
    textToInclude <- node[[colWithTextToInclude]];
    res <- gsub("(.*)<<<([^<]*)>>>(.*)",
                paste0("\\1", textToInclude, "\\3"),
                res);
  }

  return(res);

}
