processExtractedFiles <- function(path, justVerify=FALSE, silent=FALSE,
                                  encoding="UTF-8-BOM",
                                  allowedValues.variable = c("variable",
                                                             "moment",
                                                             "type",
                                                             "datatype",
                                                             "values",
                                                             "labels",
                                                             "psytype",
                                                             "dependent",
                                                             "direction",
                                                             "parent",
                                                             "description",
                                                             "comment")) {

  ### Create result object
  result <- list(input = as.list(environment()),
                 intermediate = list(),
                 output = list());

  ### Check whether 'path' exists
  if (!file.exists(path)) {
    stop("'", path, "' not found! Maybe you misspelled? Remember that in R, ",
         "you have to use forward slashes instead of backslashes regardless of ",
         "the conventions in your operating system!");
  }

  ### Store the files in 'path'
  result$intermediate$fileList <- list.files(path);

  ### Check whether we actually have files, otherwise abort
  if (length(result$intermediate$fileList) == 0) {
    stop("No files found in '", path, "' -- but it does exist.\nAre you sure it is ",
         "a directory, not a file? Maybe you misspelled? Remember that in R, ",
         "you have to use forward slashes instead of backslashes regardless of ",
         "the conventions in your operating system!");
  }

  result$intermediate$rawObjects <- list();
  result$intermediate$processedObjects <- list();
  result$intermediate$variableIndices <- list();
  result$intermediate$errors <- list();
  result$intermediate$verification <- list();
  result$output$univariate <- data.frame();
  result$output$association <- data.frame();
  result$output$variable <- data.frame();

  if (!silent) {
    cat(paste0("STARTING to process ", length(result$intermediate$fileList), " files ...\n\n"));
  }

  for (currentFile in result$intermediate$fileList) {
    if (!silent) {
      cat(paste0("Processing '", currentFile, "' ...\n"));
    }
    if (file.info(file.path(path, currentFile))$isdir) {
      if (!silent) {
        cat(paste0("... '", currentFile, "' is a directory, not processing further.\n"));
      }
      next;
    }
    foundError <- tryCatch({
      suppressWarnings(rm(res));
      ### Read the extractionscript
      extractionScript <-
        try(readLines(file.path(path, currentFile), encoding=encoding));

      ### Remove lines where extractionVerification may be called
      ### (in case somebody forgot to comment it out again after having
      ### used it)
      extractionScript <-
        extractionScript[-grep('extractionVerification', extractionScript, fixed=TRUE)];

      ### "Manually parse" the extractionscript. This way, we can remove
      ### the 'extractionVerification' (see above) and we avoid a warning
      ### if the source file has a non-standard ending.
      #source(file.path(path, currentFile), encoding=encoding);
      eval(parse(text=extractionScript));

      result$intermediate$rawObjects[[currentFile]] <- res;

      ### Remove all default bits that remained from the template (e.g.
      ### variables called 'variable name').
      res$association <-
        lapply(res$association,
               function(x) {
                 if((tolower(x$variable1)=='variablename 1') ||
                      (tolower(x$variable2)=='variablename 2')) {
                   if (!silent) {
                     cat("... For an extracted association, one of the",
                         "variable names is 'Variablename 1' or",
                         "'Variablename 2': assuming it is a leftover from the",
                         "template and deleting it.\n");
                   }
                   return(NULL);
                 } else {
                   return(x);
                 }
               });

      res$univariate <-
        lapply(res$univariate,
               function(x) {
                 if(tolower(x$variable)=='variable name') {
                   if (!silent) {
                     cat("... A univariate extraction called 'variable name'",
                         "encountered: assuming it is a leftover from the",
                         "template and deleting it.\n");
                   }
                   return(NULL);
                 } else {
                   return(x);
                 }
               });

      res$variable <-
        lapply(res$variable,
               function(x) {
                 if(tolower(x$variable)=='example') {
                   if (!silent) {
                     cat("... A measurement or manipulation (operationalisation)",
                         "has name 'example': assuming it is a leftover from the",
                         "template and deleting it.\n");
                   }
                   return(NULL);
                 } else {
                   if(!all(names(x) %IN% allowedValues.variable)) {
                     if (!silent) {
                       cat0("... Found illegal values specified in the ",
                            "variable object for variable '", x$variable, "': ",
                            vecTxt(names(x)[!(names(x) %IN% allowedValues.variable)], useQuote="'"),
                            ".\n");
                     }
                   }
                   if (sum(unlist(lapply(x, length)) > 1) >2) {
                     cat0("... Too many vectors in variable object!\n");
                   }
                   return(x);
                 }
               });

      result$intermediate$verification[[currentFile]] <-
        extractionVerification(res);
        if (!silent) {
          if (length(result$intermediate$verification[[currentFile]]$usedNotDescribed) > 0) {
            cat("... Variable(s) used (univariate or association) but not described (i.e. operationalisation / measurement missing):\n");
            cat(paste0("... ", vecTxt(result$intermediate$verification[[currentFile]]$usedNotDescribed)));
            cat("\n");
          }
          if (length(result$intermediate$verification[[currentFile]]$describedNotUsed) > 0) {
            cat("... Variable(s) described (i.e. operationalisation / measurement) but not used (univariate or association missing):\n");
            cat(paste0("... ", vecTxt(result$intermediate$verification[[currentFile]]$describedNotUsed)));
            cat("\n");
          }
          if ((length(result$intermediate$verification[[currentFile]]$usedNotDescribed) == 0) &
                (length(result$intermediate$verification[[currentFile]]$describedNotUsed) == 0)) {
            cat(paste0("... Verified extraction file without errors!\n"));
          }
        }
    },
    error = function(e) {
      return(paste0("... Error reading source file of '", currentFile, "': ", e, "\n"));
      if (!silent) {
        cat(paste0("... Error reading source file of '", currentFile, "': ", e, "\n"));
      }
    });

    ### Only continue if the file was read correctly
    if (is.null(foundError)) {

      if (!silent) {
        cat(paste0("... Finished reading source file of '", currentFile, "'\n"));
      }

      ### Start reorganising information in this object; it is useful to have
      ### the names available. The name can be stored as 'subsample' or as 'name'.
      if (is.null(names(res$subsample))) {
        subsampleNames <- unlist(lapply(res$subsample, FUN=function(x){
          if (!is.null(x$subsample)) {
            return(x$subsample);
          } else {
            return(x$name);
          }
        }));
        names(res$subsample) <- subsampleNames;
      } else {
        subsampleNames <- names(res$subsample);
      }

      #######################################################################
      #######################################################################
      ### CREATE INDEX FOR EASILY ACCESSING VARIABLE (OPERATIONALISATION)
      ### INFORMATION
      #######################################################################
      #######################################################################

      result$intermediate$variableIndices[[currentFile]] <-
        unlist(lapply(res$variable, function(x) {
          return(x$variable);
        }));

      if (!silent) {
        cat(paste0("... File contains variables '",
                   vecTxt(result$intermediate$variableIndices[[currentFile]],
                          delimiter="', '", lastDelimiter="' & '"),
                   "'.\n"));
      }

      #######################################################################
      #######################################################################
      ### PROCESS THE EXTRACTED ASSOCIATIONS
      #######################################################################
      #######################################################################

      ### Convert the extracted associations to effect sizes we can use

      ### First convert all objects to single values of the corresponding
      ### effectsize
      res$association <- lapply(res$association, function(x) {
        ### Convert all names to lowercase.
        ### names(x) <- tolower(names(x));

        for (currentObjectName in names(x)) {
          if (grepl('raw\\.|crosstable', currentObjectName)) {
            if (currentObjectName %IN% c("raw.table", "crosstable")) {
              ### First check whether this isn't an empty table leftover from
              ### th template
              if (identical(x[[currentObjectName]], matrix(c(NA, NA,
                                         NA, NA),
                                       ncol=2,
                                       byrow=TRUE))) {
                ### If it is empty, remove it
                x[[currentObjectName]] <- NULL;
              } else {
                if ('V' %IN% names(x)) {
                  newName <- 'v.from.raw.table';
                } else {
                  newName <- 'v';
                }
                x[[newName]] <-
                  cramersV(x[[currentObjectName]])$output$cramersV;
                x[[currentObjectName]] <- NULL;
              }
            }
            if(currentObjectName == "raw.chisq") {
              if ('V' %IN% names(x)) {
                newName <- 'v.from.raw.chisq';
              } else {
                newName <- 'v';
              }
              x[[newName]] <-
                convert.chisq.to.V(x[[currentObjectName]]$statistic,
                                   n=sum(x[[currentObjectName]]$observed),
                                   minDim=min(dim(x[[currentObjectName]]$observed)));
              x[[currentObjectName]] <- NULL;
            }
          }
        }
        return(x)
      });

      ### Store in our results object
      result$intermediate$processedObjects[[currentFile]] <- res;

      ### At this point, only single values should be left. In addition,
      ### we converted all names to lowercase.

      ### Therefore, now convert all statistics into related effect sizes

  #     ### Only for quantitative studies reporting associations
  #     if (length(res$association) > 0) {
  #       for (curAssocIndex in 1:length(res$association)) {
  #
  #         ### First check whether this association relates to the entire
  #         ### sample or so a subsample; then set the sample size for this
  #         ### association
  #         if (!is.null(res$association[[curAssocIndex]]$subsample) &&
  #               !is.na(res$association[[curAssocIndex]]$subsample) &&
  #               !is.null(res$subsample[[res$association[[curAssocIndex]]$subsample]]) &&
  #               !is.na(res$subsample[[res$association[[curAssocIndex]]$subsample]])) {
  #           n <- res$subsample[[res$association[[curAssocIndex]]$subsample]]$size;
  #         } else if (!is.null(res$sample$size) && !is.na(res$sample$size)) {
  #           n <- res$sample$size;
  #         } else {
  #           n <- NA;
  #         }
  #
  #         ### Overwrite if we have an n specified for this sample
  #         ### Check whether we also have the sample size
  #         if ('n' %IN% names(res$association[[curAssocIndex]]) &&
  #               !is.na(res$association[[curAssocIndex]]$n) &&
  #               is.numeric(res$association[[curAssocIndex]]$n)) {
  #           n <- res$association[[curAssocIndex]]$n;
  #         }
  #
  #         res$association[[curAssocIndex]] <-
  #           convertToEffectsizes(object = res$association[[curAssocIndex]],
  #                                n = n, silent = silent);
  #
  #       }
  #     }

      #######################################################################
      #######################################################################
      ### CREATE NEW DICHOTOMOUS VARIABLES (OPERATIONALISATIONS) FOR
      ### COMPARISONS OF TWO LEVELS OF CATEGORICAL VARIABLES
      #######################################################################
      #######################################################################

      ### Only for quantitative studies reporting associations
      if (length(res$association) > 0) {
        ### Check all reported associations
        for (curAssocIndex in 1:length(res$association)) {
          ### Check whether values are specified for each of the two
          ### variables (because these values represent two out of more
          ### levels/categories for a categorical variable)
          for (curVar in 1:2) {
            ### Set names for values variable and variable name variable
            curValuesVarName <- paste0('var', curVar, 'values');
            curVarName <- paste0('variable', curVar);

            if (!is.null(res$association[[curAssocIndex]][[curValuesVarName]]) &&
                  !is.na(res$association[[curAssocIndex]][[curValuesVarName]])) {
              ### Get index of current variable in the variables object
              varIndex <-
                which(result$intermediate$variableIndices[[currentFile]] ==
                        res$association[[curAssocIndex]][[curVarName]]);

              ### Get the values and labels of this variable
              varValues <- result$intermediate$rawObjects[[currentFile]]$variable[[varIndex]]$values;
              varLabels <- result$intermediate$rawObjects[[currentFile]]$variable[[varIndex]]$labels;

              ### Extract the values and variables that are used in the
              ### current association
              usedVarValues <-
                which(res$association[[curAssocIndex]][[curValuesVarName]] %in% varValues);
              usedVarLabels <- varLabels[usedVarValues];

              ### Construct the name of the new variable to create
              newVariableName <-
                paste0(res$association[[curAssocIndex]][[curVarName]],
                       " (", vecTxt(usedVarLabels), ")");

              ### Check whether we didn't create this variable yet;
              ### otherwise, create it.
              if (!any(newVariableName %in% result$intermediate$variableIndices[[currentFile]])) {
                ### Copy the entire 'complete' variable to a new slot in the
                ### operationalisations list
                newVariableIndex <- length(res$variable) + 1;
                res$variable[[newVariableIndex]] <-
                  result$intermediate$rawObjects[[currentFile]]$variable[[varIndex]];
                ### Change its name
                res$variable[[newVariableIndex]]$variable <-
                  paste0(res$variable[[newVariableIndex]]$variable,
                         " (", vecTxt(usedVarLabels), ")");
                ### Update the values and labels
                res$variable[[newVariableIndex]]$values <- usedVarValues;
                res$variable[[newVariableIndex]]$labels <- usedVarLabels;
                ### Indicate the parent variable
                res$variable[[newVariableIndex]]$parent <-
                  res$association[[curAssocIndex]][[curVarName]];
                ### Add to list with variable names
                result$intermediate$variableIndices[[currentFile]] <-
                  c(result$intermediate$variableIndices[[currentFile]],
                    newVariableName);
              } else {
                newVariableIndex <-
                  which(result$intermediate$variableIndices[[currentFile]] == newVariableName);
              }

              ### Then change the relevant association, replacing the
              ### variable with the new dichotomous variable, and then
              ### removing the values (as this no longer needs to be specified)
              res$association[[curAssocIndex]][[curVarName]] <- newVariableName;
              res$association[[curAssocIndex]][[curValuesVarName]] <- NULL;

            }
          }
        }
      }

      #######################################################################
      ### DELETE EFFECTIVELY EMPTY SUB-OBJECTS
      #######################################################################

      res <- lapply(res, function(x) {
        if (is.null(unlist(x))) {
          return(list());
        } else {
          return(x);
        }
      });

      ### Store in our results object
      result$intermediate$processedObjects[[currentFile]] <- res;

      #######################################################################
      #######################################################################
      ### STORE A DATAFRAME WITH THE SUBSAMPLES FOR THIS OBJECT
      #######################################################################
      #######################################################################

      ### Start adding information to aggregated dataframes

      tryCatch({

        ### Check whether all univariate values are vectors of length one
        if (sum(unlist(lapply(res$univariate,
                              function(x) {
                                return(sum(unlist(lapply(x, function(x) {
                                  return(length(x)>1);
                                })))>0);
                              })))>0) {
          stop("One of the univariate values is a vector - only single values can be specified here!");
        }
        ### Check whether all bivariate values are vectors of length one
        if (sum(unlist(lapply(res$association,
                              function(x) {
                                return(sum(unlist(lapply(x, function(x) {
                                  return(length(x)>1);
                                })))>0);
                              })))>0) {
          stop("One of the association values is a vector - only single values can be specified here!");
        }

        ### Note that we collapse any vectors that we find in the variables,
        ### so we don't have to check this there.
        if (length(res$univariate) > 0) {
          result$output$univariate <-
            rbind.fill(result$output$univariate,
                       cbind(data.frame(study = rep(currentFile, length(res$univariate))),
                             ldply(res$univariate, function(x) {
                               return(as.data.frame(x, stringsAsFactors = FALSE));
                             })));
          if (!silent) {
            cat(paste0("... Combining with aggregate: univariate added"));
          }
        } else {
          if (!silent) {
            cat(paste0("... Combining with aggregate: univariate skipped (0 lines)"));
          }
        }

        if (length(res$association) > 0) {
          result$output$association <-
            rbind.fill(result$output$association,
                       cbind(data.frame(study = rep(currentFile, length(res$association))),
                             ldply(res$association, function(x) {
                               return(as.data.frame(x, stringsAsFactors = FALSE));
                             })));
          if (!silent) {
            cat(paste0(", association added"));
          }
        } else {
          if (!silent) {
            cat(paste0(", association skipped (0 lines)"));
          }
        }

        if (length(res$variable) > 0) {
          result$output$variable <-
            rbind.fill(result$output$variable,
                       cbind(data.frame(study = rep(currentFile, length(res$variable))),
                             ldply(res$variable, function(x) {
                               return(as.data.frame(lapply(x, FUN=paste0, collapse=" | "), stringsAsFactors = FALSE));
                             })));
          if (!silent) {
            cat(paste0(" & variable added.\n"));
          }
        } else {
          if (!silent) {
            cat(paste0(" & variable skipped (0 lines)\n"));
          }
        }
      },
      error = function(e) {
        e <- sub("Error in doTryCatch\\(return\\(expr\\), name, parentenv, handler\\): (.*)",
                 "\\1", e);
        result$intermediate$errors[[currentFile]] <- e;
        if (!silent) {
          cat(paste0("... Error processing data in '", currentFile, "': ", e, "\n"));
        }
      });


      if (!silent) {
        cat(paste0("Finished processing '", currentFile, "'\n\n"));
      }
    } else {
      result$intermediate$errors[[currentFile]] <- foundError;
      if (!silent) {
        cat(foundError);
        cat(paste0("Aborted processing '", currentFile, "'\n\n"));
      }
    }
  }

  ### Add unique identifyer for each variable
  result$output$variable$uniqueVarID <-
    uniqueVarID(result$output$variable$study,
                result$output$variable$variable);

  if (!silent && any(duplicated(result$output$variable$uniqueVarID))) {
    cat0("WARNING: duplicate variables detected: ",
         vecTxt(result$output$variable$uniqueVarID[duplicated(result$output$variable$uniqueVarID)]),
         "\n\n");
  }

  if (!silent) {
    cat(paste0("FINISHED processing ",
               length(result$intermediate$processedObjects),
               " files.\n\n"));
  }

  return(result);
}
