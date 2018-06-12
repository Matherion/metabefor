rxs_fromSpecifications <- function(gs_url = NULL,
                                   ws = list(entities = 'entities',
                                             valueTemplates = 'valueTemplates',
                                             definitions = 'definitions'),
                                   entitiesFilename = NULL,
                                   valueTemplatesFilename = NULL,
                                   definitionsFilename = NULL,
                                   gs_localBackup = list(entities = NULL,
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
                                   silent=FALSE) {

  ### Check argument integrity
  if (!is.null(entitiesFilename)) {
    if (!file.exists(entitiesFilename)) {
      stop("You specified a filename for 'entitiesFilename' ('",
           entitiesFilename, "'), but it does not exist.");
    }
  }
  if (!is.null(valueTemplatesFilename)) {
    if (!file.exists(valueTemplatesFilename)) {
      stop("You specified a filename for 'valueTemplatesFilename' ('",
           valueTemplatesFilename, "'), but it does not exist.");
    }
  }

  ### Import sheets, if sheets identifier (gs_url) was provided
  entities <- FALSE;
  if (!is.null(gs_url)) {
    tryCatch({
      gsObject <- gs_url(gs_url);
      entities <- gs_read(gsObject, ws = ws$entities);
      valueTemplates <- gs_read(gsObject, ws = ws$valueTemplates);
      if (!is.null(ws$definitions)) {
        definitions <- gs_read(gsObject, ws = ws$definitions);
      }
    },
             error = function(e) {
               cat("\nYou specified a google sheet, but I have problems",
                   "accessing it - trying to access local files.\n");
             });
  }

  ### If the sheets identifier was not provided, or loading it failed,
  ### load from a local file
  if (all(entities == FALSE)) {
    if (is.null(entitiesFilename)) {
      stop("Either a google sheets URL was not provided in gs_url, ",
           "or loading the sheets failed; and you did not provide ",
           "a filename in 'entitiesFilename'. That means that I cannot ",
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
  }

  ### Write local backup, if need be
  if (!is.null(gs_localBackup$entities)) {
    write.csv(entities,
              row.names=FALSE,
              gs_localBackup$entities);
  }
  if (!is.null(gs_localBackup$valueTemplates)) {
    write.csv(valueTemplates,
              row.names=FALSE,
              gs_localBackup$valueTemplates);
  }
  if (!is.null(gs_localBackup$definitions) && !is.null(definitions)) {
    write.csv(definitions,
              row.names=FALSE,
              gs_localBackup$definitions);
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

  if (!is.null(outputFile)) {
    if (isTRUE(outputFile)) {
      ### Write to current working directory
      writeLines(paste0(unlist(rxsTemplate), collapse="\n"),
                 "template.rxs.Rmd");
    } else if (is.character(outputFile)) {
      writeLines(paste0(unlist(rxsTemplate), collapse="\n"),
                 outputFile);
    }
    invisible(rxsTemplate);
  } else {
    return(rxsTemplate);
  }

}
