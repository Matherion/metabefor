rxs_fg_container <- function(node,
                             valueTemplates,
                             indent = TRUE,
                             indentSpaces = 2,
                             fullWidth = 80,
                             commentCharacter = "#",
                             fillerCharacter = "#",
                             eC = entityColNames(),
                             repeatingSuffix = "__1__",
                             silent=FALSE,
                             overrideLevel = NULL) {

  ### A container that does itself not contain extractable entities,
  ### but which does contains other nodes that contain extractable entities.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argument 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(parsedValueTemplates)), ").");
  }

  if (is.numeric(overrideLevel)) {
    level <- overrideLevel;
  } else {
    level <- node$level;
    if (!is.null(overrideLevel) && !silent) {
      cat0("Argument 'overrideLevel' is not NULL but also not numeric; ",
           "ignoring it and setting the level to this node's level ");
    }
  }

  lV <- rxs_fg_layoutVars(level = level,
                          indent = indent,
                          indentSpaces = indentSpaces,
                          fullWidth = fullWidth,
                          commentCharacter = commentCharacter,
                          fillerCharacter = fillerCharacter);

  if (isTRUE(node[[eC$repeatingCol]])) {
    currentEntityName <- paste0(node$name, repeatingSuffix);
    currentStartEndName <- paste0(node$name, " (REPEATING)");
  } else {
    currentEntityName <- node$name;
    currentStartEndName <- node$name;
  }

  childAddition <- paste0(lV$indentSpaces,
                          returnPathToRoot(node$parent),
                          "$AddChild('",
                          currentEntityName,
                          "');");

  titleDescription <-
    rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                            description=node[[eC$descriptionCol]],
                            level=level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);

  openingTxt <- paste0(" START: ", currentStartEndName, " ");
  closingTxt <- paste0(" END: ", currentStartEndName, " ");
  openingTxt <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(openingTxt) - 4, fillerCharacter),
           openingTxt,
           repStr(fillerCharacter, 3));
  closingTxt <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(closingTxt) - 4, fillerCharacter),
           closingTxt,
           repStr(fillerCharacter, 3));

  ### Now get the character vectors for the children
  childFragments <- unlist(sapply(node$children,
                                  function(node) {
                                    return(c(lV$indentSpaces,
                                             rxs_fg_dispatcher(node,
                                                               valueTemplates = valueTemplates),
                                             lV$indentSpaces));
                                  }));

  ### Return the result in a list in case we're called for multiple nodes

  return(list(c(lV$lineFiller,
                openingTxt,
                lV$lineFiller,
                childAddition,
                #assignmentToChild,
                titleDescription,
                #lV$valuePrefix,
                #valueAssignment,
                #lV$valuePrefix,
                #lV$lineFiller,
                lV$valuePrefix,
                childFragments,
                lV$valuePrefix,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
