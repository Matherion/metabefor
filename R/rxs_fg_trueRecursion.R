rxs_fg_trueRecursion <- function(node,
                                 valueTemplates,
                                 indent = TRUE,
                                 indentSpaces = 2,
                                 fullWidth = 80,
                                 commentCharacter = "#",
                                 fillerCharacter = "#",
                                 eC = entityColNames(),
                                 repeatingSuffix = "__1__",
                                 silent=FALSE,
                                 overrideLevel = NULL,
                                 trueRecursionText = getOption("metabefor_trueRecursionText",
                                                               rxs_fg_trueRecursionText)) {

  ### This is a recursive occurrence of a recurring node.
  ### Therefore, we notify the coder that they should copy-paste
  ### the recursive node from the bottom.

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

  assignmentToChild <- paste0(lV$indentSpaces,
                              "recursiveElementPlaceholder_level1 <- ",
                              returnPathToRoot(node),
                              ";");

  titleDescription <-
    rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                            description=node[[eC$descriptionCol]],
                            level=level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);

  valueAssignment <- c(paste0(lV$indentSpaces, "NA"),
                       lV$indentSpaces,
                       strwrap(trueRecursionText(recursiveNodeName = node[[eC$recursingCol]]),
                               width=lV$commentWidth,
                               prefix=paste0(lV$commentPrefix)));

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

  ### Return the result in a list in case we're called for multiple nodes
  return(list(c(lV$lineFiller,
                openingTxt,
                lV$lineFiller,
                childAddition,
                assignmentToChild,
                titleDescription,
                lV$valuePrefix,
                valueAssignment,
                lV$valuePrefix,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
