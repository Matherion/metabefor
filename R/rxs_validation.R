rxs_validation <- function(studyTree,
                           rxsStructure=NULL) {
  studyTree$Set(validationResults = paste0("Validation run starting at ",
                                           format(Sys.time(), "%Y-%m-%d %H:%S")));
  studyTree$Do(function(node) {
    if (is.list(node$value)) {
      ### Loop through the elements
      listValidationMessages <-
          sapply(names(node$value), function(valueName) {
            relevantNode <- NULL;
            if (!is.null(node$validation[[valueName]])) {
              VALUE <- node$value[[valueName]];
              errorMsg <- "";
              validationOutcome <- eval(node$validation[[valueName]]);
              if (is.na(validationOutcome)) {
                errorMsg <- paste0("Validation code for value '",
                                   valueName,
                                   "' in node '",
                                   node$name,
                                   "' evaluated to NA.");
              } else if (is.logical(validationOutcome) && !validationOutcome) {
                if (!is.null(rxsStructure)) {
                  ### Find relevant node (entity) and get the valueTemplate
                  ### Then get the error message to use.
                  relevantNode <-
                    FindNode(rxsStructure$parsedEntities$extractionScriptTree,
                             valueName);
                  if (is.null(relevantNode)) {
                    ### Either a node with this name does not exist,
                    ### or it's a recursive node
                    relevantNode <-
                      FindNode(rxsStructure$parsedEntities$recursingNodes,
                               valueName);
                  }

                  if (!is.null(relevantNode)) {
                    valueTemplateName <- relevantNode[[eC$valueTemplateCol]];
                    errorMsg <-
                      rxsStructure$parsedValueTemplates[[valueTemplateName]]$error;
                    if (is.null(errorMsg) ||
                        is.na(errorMsg) ||
                        (nchar(trim(errorMsg)) == 0)) {
                      errorMsg <- "";
                    } else {
                      errorMsg <- gsub('NAME',
                                       valueName,
                                       errorMsg);
                    }
                  }
                }
                if (nchar(trim(errorMsg)) > 0) {
                  errorMsg <-
                    paste0(": ", errorMsg);
                }
                return(paste0("Failed validation for '",
                              valueName,
                              "'",
                              errorMsg));
              } else {
                return (NULL);
              }
            } else {
              return(NULL);
            }
          });
      node$Set(validationResults =
                 list(c(node$root$validationResults,
                        listValidationMessages)),
               filterFun = isRoot,
               traversal = 'ancestor');
    } else if (is.expression(node$validation)) {
      VALUE <- node$value;
      errorMsg <- "";
      validationOutcome <- eval(node$validation);
      if (is.logical(validationOutcome) && !validationOutcome) {
        if (!is.null(rxsStructure)) {
          ### Find relevant node (entity) and get the valueTemplate
          ### Then get the error message to use.
          relevantNode <-
            FindNode(rxsStructure$parsedEntities$extractionScriptTree,
                     node$name);
          if (is.null(relevantNode)) {
            ### Either a node with this name does not exist,
            ### or it's a recursive node
            relevantNode <-
              FindNode(rxsStructure$parsedEntities$recursingNodes,
                       node$name);
          }
          if (is.null(relevantNode)) {
            valueTemplateName <- relevantNode[[eC$valueTemplateCol]];
            errorMsg <-
              rxsStructure$parsedValueTemplates[[valueTemplateName]]$error;
            if (is.null(errorMsg) ||
                is.na(errorMsg) ||
                (nchar(trim(errorMsg)) == 0)) {
              errorMsg <- "";
            } else {
              errorMsg <- gsub('NAME',
                               valueName,
                               errorMsg);
            }
          }
        }
        if (nchar(trim(errorMsg)) > 0) {
          errorMsg <-
            paste0(": ", errorMsg);
        }
        validationMsg <- paste0("Failed validation for '",
                                node$name, "'",
                                errorMsg);
        node$Set(validationResults =
                   list(c(node$root$validationResults,
                          validationMsg)),
                 filterFun = isRoot,
                 traversal = 'ancestor');
        invisible(NULL);
      }
    } else {
      invisible(NULL);
    }
  });
  studyTree$Set(validationResults =
                  list(c(studyTree$validationResults,
                         paste0("Validation run ending at ",
                                format(Sys.time(), "%Y-%m-%d %H:%S")))),
                filterFun = isRoot);
  studyTree$validationResults <- unlist(studyTree$validationResults);
}
