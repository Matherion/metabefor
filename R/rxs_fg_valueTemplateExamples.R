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

  res <- NA;

  if (!is.null(valueTemplate$examples) && !is.na(valueTemplate$examples) && !(nchar(valueTemplate$examples) == 0)) {
    res <- valueTemplate$examples;
  }

  ### Possibly override with value from entity specification
  if (!is.null(node$examples) && !is.na(node$examples) && !(nchar(node$examples) == 0)) {
    res <- node$examples;
  }

  ### No example provided
  if (is.na(res)) {
    return(NULL);
  }

  ### Sanitize returns and multiple whitespaces
  res <- gsub('\\n', ' ', res);

  # ### Do fieldname replacement using regular expressions, if need be
  # allEntityFieldNames <- paste0("<<", eC, ">>");
  # allValueTemplateFieldNames <- paste0("<<", valueTemplateColNames(), ">>");
  #
  # fieldNameReplacementHits <- sapply(allEntityFieldNames, grepl, x=res);
  # if (any(fieldNameReplacementHits)) {
  #   for (i in which(fieldNameReplacementHits)) {
  #     fieldNameReplacementContents <-
  #       node[[eC[[i]]]];
  #     res <- gsub(allEntityFieldNames[i],
  #                 fieldNameReplacementContents,
  #                 res);
  #   }
  # }

  ######################################################################
  ### Do fieldname replacement using regular expressions, if need be
  ######################################################################

  ### First see whether we have any hits in the entity specification,
  ### which would override anything in the value template
  allEntityFieldNames <- paste0("<<", eC, ">>");
  matchesInEntityFieldNames <-
    which(sapply(allEntityFieldNames, grepl, x=res));
  entityFieldValues <- c();
  if (any(matchesInEntityFieldNames)) {
    if (!is.null(node[[unlist(eC[matchesInEntityFieldNames])]])) {
      entityFieldValues <- node[[unlist(eC[matchesInEntityFieldNames])]];
      names(entityFieldValues) <- unlist(eC[matchesInEntityFieldNames]);
    }
  }

  ### Then look in the value template specification
  allValueTemplateFieldNames <- paste0("<<", valueTemplateColNames(), ">>");
  matchesInValueTemplateFieldNames <-
    sapply(allValueTemplateFieldNames, grepl, x=res);
  if (any(matchesInValueTemplateFieldNames)) {
    valueTemplateFieldValues <-
      valueTemplate[[unlist(valueTemplateColNames()[matchesInValueTemplateFieldNames])]];
    names(valueTemplateFieldValues) <-
      valueTemplateColNames()[matchesInValueTemplateFieldNames]
  } else {
    valueTemplateFieldValues <- c();
  }

  ### If we have no matches, we can immediately return the
  ### answer without replacing anything
  if (length(c(entityFieldValues, valueTemplateFieldValues)) > 0) {

    ### Now, for both lists, remove all matched fields that don't have a value
    entityFieldValues <-
      ifelse(!is.null(entityFieldValues) &
               !is.na(entityFieldValues) &
               (length(entityFieldValues) > 0),
             entityFieldValues,
             NA);
    entityFieldValues <-
      entityFieldValues[!is.na(entityFieldValues)];

    valueTemplateFieldValues <-
      ifelse(!is.null(valueTemplateFieldValues) &
               !is.na(valueTemplateFieldValues) &
               (length(valueTemplateFieldValues) > 0),
             valueTemplateFieldValues,
             NA);
    valueTemplateFieldValues <-
      valueTemplateFieldValues[!is.na(valueTemplateFieldValues)];

    ### Now remove all remaining fields, that apparently have valid
    ### values for the entity sheet (which overrides value templates),
    ### from the value templates list.
    valueTemplateFieldValuesToKeep <-
      setdiff(names(valueTemplateFieldValues),
              names(entityFieldValues));
    valueTemplateFieldValues <-
      valueTemplateFieldValues[valueTemplateFieldValuesToKeep];

    ### Concatenate both lists and process them
    fullReplacementList <- c(entityFieldValues,
                             valueTemplateFieldValues);

    ### Process list
    for (i in seq_along(fullReplacementList)) {
      if (grepl("\\|\\|", fullReplacementList[i])) {
        ### It's multiple elements, so change it into a valid vector
        fieldNameReplacementContents <-
          paste0("c(",
                 paste0(trim(unlist(strsplit(fullReplacementList[i],
                                             "||", fixed=TRUE))),
                        collapse=", "),
                 ")");
      } else {
        fieldNameReplacementContents <- fullReplacementList[i];
      }

      ### Do the replacement for this field
      res <- gsub(paste0("<<", names(fullReplacementList)[i], ">>"),
                  fieldNameReplacementContents,
                  res);
    }
  }


  ### Probably have to take the section from the valueTemplateValidation code.




  if (listVersion) {
    # lV <- rxs_fg_layoutVars(level = level,
    #                         indent = indent,
    #                         indentSpaces = indentSpaces,
    #                         fullWidth = fullWidth,
    #                         commentCharacter = commentCharacter,
    #                         fillerCharacter = fillerCharacter);
    # res <- paste0(lV$commentPrefix,
    #               trim(unlist(strsplit(res, "||", fixed=TRUE))));
    res <- trim(unlist(strsplit(res, "||", fixed=TRUE)));
    return(res);
  } else {
    lV <- rxs_fg_layoutVars(level = level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);
    res <- paste0(lV$commentPrefix,
                  trim(unlist(strsplit(res, "||", fixed=TRUE))));
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
