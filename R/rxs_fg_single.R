rxs_fg_single <- function(node,
                          valueTemplates,
                          indent = TRUE,
                          indentSpaces = 2,
                          fullWidth = 80,
                          commentCharacter = "#",
                          fillerCharacter = "#",
                          repeatingSuffix = "__1__",
                          eC = entityColNames(),
                          silent=FALSE,
                          overrideLevel = NULL) {

  ### Simplest situation; we'll just return a full, simple
  ### fragment for this entity alone.

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
                              returnPathToRoot(node),
                              "[['value']] <-");

  titleDescription <-
    rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                            description=node[[eC$descriptionCol]],
                            level=level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);

  valueAssignment <-
    rxs_fg_defaultValueAssignment(node=node,
                                  valueTemplates = valueTemplates,
                                  level = level,
                                  indent = indent,
                                  indentSpaces = indentSpaces,
                                  fullWidth = fullWidth,
                                  commentCharacter = commentCharacter,
                                  fillerCharacter = fillerCharacter,
                                  eC = eC);

  descriptionExamplesHeader <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(" VALUE DESCRIPTION AND EXAMPLES ") - 4, fillerCharacter),
           " VALUE DESCRIPTION AND EXAMPLES ",
           repStr(fillerCharacter, 3));

  valueTemplateDescription <-
    rxs_fg_valueTemplateDescription(node=node,
                                    valueTemplates = valueTemplates,
                                    level = level,
                                    indent = indent,
                                    indentSpaces = indentSpaces,
                                    fullWidth = fullWidth,
                                    commentCharacter = commentCharacter,
                                    fillerCharacter = fillerCharacter,
                                    eC = eC);

  valueTemplateExamples <-
    rxs_fg_valueTemplateExamples(node=node,
                                 valueTemplates = valueTemplates,
                                 level = level,
                                 indent = indent,
                                 indentSpaces = indentSpaces,
                                 fullWidth = fullWidth,
                                 commentCharacter = commentCharacter,
                                 fillerCharacter = fillerCharacter,
                                 eC = eC);

  valueTemplateValidation <-
    rxs_fg_valueTemplateValidation(node=node,
                                   valueTemplates = valueTemplates,
                                   level = level,
                                   indent = indent,
                                   indentSpaces = indentSpaces,
                                   fullWidth = fullWidth,
                                   commentCharacter = commentCharacter,
                                   fillerCharacter = fillerCharacter,
                                   eC = eC);

  ### Use 'this' - process afterwards using data.tree - or some other
  ### way to leverage data.tree's "Do"



  validationSpecification <-
    paste0(lV$indentSpaces,
           returnPathToRoot(node),
           "[['validation']] <- expression(",
           valueTemplateValidation,
           ");");

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
                descriptionExamplesHeader,
                lV$lineFiller,
                lV$commentPrefix,
                valueTemplateDescription,
                lV$commentPrefix,
                valueTemplateExamples,
                lV$commentPrefix,
                lV$lineFiller,
                validationSpecification,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
