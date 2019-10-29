rxs_fromSpecifications <- function(gs_url = NULL,
                                   ws = list(entities = 'entities',
                                             valueTemplates = 'valueTemplates',
                                             definitions = 'definitions'),
                                   entitiesFilename = NULL,
                                   valueTemplatesFilename = NULL,
                                   definitionsFilename = NULL,
                                   localBackup = list(entities = NULL,
                                                      valueTemplates= NULL,
                                                      definitions = NULL),
                                   outputFile = NULL,
                                   yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                       author = NULL,
                                                       date = format(Sys.time(), '%d %b %Y at %H:%M:%S')),
                                   author = NULL,
                                   indent = TRUE,
                                   indentSpaces = 2,
                                   fullWidth = 80,
                                   commentCharacter = "#",
                                   fillerCharacter = "#",
                                   eC = entityColNames(),
                                   valueTemplateCols = valueTemplateColNames(),
                                   repeatingSuffix = "__1__",
                                   rootName = "study",
                                   silent=FALSE,
                                   returnFullObject = FALSE) {

  ### Import sheets, if sheets identifier (gs_url) was provided
  entities <- FALSE;
  definitions <- NULL; ### In case the full object is requested but
                       ### no definitions are loaded
  if (!is.null(gs_url)) {
    tryCatch({
      gsObject <- gs_url(gs_url);
      entities <- gs_read(gsObject, ws = ws$entities);
      valueTemplates <- gs_read(gsObject, ws = ws$valueTemplates);
      if (!is.null(ws$definitions)) {
        definitions <- gs_read(gsObject, ws = ws$definitions);
      }
      if (!silent) {
        cat("Successfully read the extraction script specifications from Google sheets.\n");
      }
    },
             error = function(e) {
               if (!silent) {
                 cat("You specified a google sheet, but I have problems",
                     "accessing it - trying to access local files.\n");
               }
               if (getOption("metabefor.debug", FALSE)) {
                 ufs::cat0("Error message:\n  ",
                           e$message);
               }
             });
  }

  ### If the sheets identifier was not provided, or loading it failed,
  ### load from a local file
  if (all(entities == FALSE)) {

    ### Check whether the files exist
    if (!is.null(entitiesFilename)) {
      if (!file.exists(entitiesFilename)) {
        stop("You specified a filename for 'entitiesFilename' ('",
             entitiesFilename, "'), but it does not exist.");
      }
    } else {
      stop("Either a google sheets URL was not provided in gs_url, ",
           "or loading the sheets failed; and you did not provide ",
           "a filename in 'entitiesFilename'. That means that I cannot ",
           "load the extraction script specifications.");
    }

    if (!is.null(valueTemplatesFilename)) {
      if (!file.exists(valueTemplatesFilename)) {
        stop("You specified a filename for 'valueTemplatesFilename' ('",
             valueTemplatesFilename, "'), but it does not exist.");
      }
    } else {
      stop("Either a google sheets URL was not provided in gs_url, ",
           "or loading the sheets failed; and you did not provide ",
           "a filename in 'valueTemplatesFilename'. That means that I cannot ",
           "load the extraction script specifications.");
    }

    entities <- read.csv(entitiesFilename,
                         stringsAsFactors = FALSE);
    valueTemplates <- read.csv(valueTemplatesFilename,
                               stringsAsFactors = FALSE);
    if (!is.null(definitionsFilename)) {
      definitions <- read.csv(definitionsFilename,
                              stringsAsFactors = FALSE);
    }

    if (!silent) {
      cat("Succesfully read the extraction script specifications from local files.\n");
    }

  }

  ### Write local backup, if need be
  if (!is.null(localBackup$entities)) {
    write.csv(entities,
              row.names=FALSE,
              localBackup$entities);
    if (!silent) {
      cat0("Stored local backup of entities to '", localBackup$entities, "'.\n");
    }
  }
  if (!is.null(localBackup$valueTemplates)) {
    write.csv(valueTemplates,
              row.names=FALSE,
              localBackup$valueTemplates);
    if (!silent) {
      cat0("Stored local backup of value templates to '", localBackup$valueTemplates, "'.\n");
    }
  }
  if (!is.null(localBackup$definitions) && !is.null(definitions)) {
    write.csv(definitions,
              row.names=FALSE,
              localBackup$definitions);
    if (!silent) {
      cat0("Stored local backup of definitions to '", localBackup$definitions, "'.\n");
    }
  }

  ### Finally start processing
  rxsStructure <- rxs_parseSpecifications(entities = entities,
                                          valueTemplates = valueTemplates,
                                          definitions = definitions,
                                          eC = eC,
                                          valueTemplateCols = valueTemplateCols,
                                          rootName = rootName);

  rxsTemplate <- rxs_buildTemplate(rxsStructure = rxsStructure,
                                   yamlMetadata = yamlMetadata,
                                   indent = indent,
                                   indentSpaces = indentSpaces,
                                   fullWidth = fullWidth,
                                   commentCharacter = commentCharacter,
                                   fillerCharacter = fillerCharacter,
                                   eC = eC,
                                   repeatingSuffix = repeatingSuffix,
                                   silent=silent);
  if (!silent) {
    cat("Parsed extraction script specifications into extraction script template.\n");
  }

  if (returnFullObject) {
    res <- list(rxsSpecification = list(entities = entities,
                                        valueTemplates = valueTemplates,
                                        definitions = definitions,
                                        eC = eC,
                                        valueTemplateCols = valueTemplateCols,
                                        rootName = rootName,
                                        yamlMetadata = yamlMetadata,
                                        indent=indent,
                                        indentSpaces=indentSpaces,
                                        commentCharacter = commentCharacter,
                                        fillerCharacter = fillerCharacter),
                rxsStructure = rxsStructure,
                rxsTemplate = rxsTemplate);
    class(res) <- "rxsStructure";
  } else {
    res <- rxsTemplate;
  }

  if (!is.null(outputFile)) {
    if (isTRUE(outputFile)) {
      ### Write to current working directory
      fileToWriteTo <- file.path(getwd(), "template.rxs.Rmd");
    } else if (is.character(outputFile)) {
      ### Path is specified in 'outputFile'
      fileToWriteTo <- outputFile;
    }
    writeLines(paste0(unlist(rxsTemplate), collapse="\n"),
               fileToWriteTo);
    if (!silent) {
      cat0("Successfully wrote extraction script template to '",
           fileToWriteTo, "'.\n");
    }
    invisible(res);
  } else {
    return(res);
  }

}
