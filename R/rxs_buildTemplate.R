rxs_buildTemplate <- function(rxsStructure,
                              yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                  author = NULL,
                                                  date = format(Sys.time(), '%d %b %Y at %H:%M:%S')),
                              gs_url = NULL,
                              indent = TRUE,
                              indentSpaces = 2,
                              fullWidth = 80,
                              commentCharacter = "#",
                              fillerCharacter = "#",
                              eC = entityColNames(),
                              repeatingSuffix = "__1__",
                              silent=FALSE) {

  if (!("rxsStructure" %IN% class(rxsStructure))) {
    stop("The class of the object provided as argument 'rxsStructure' is not ",
         "'rxsStructure' (but instead ", vecTxtQ(rxsStructure), ").");
  }

  scriptChunk <-
    rxs_fg_dispatcher(node = rxsStructure$parsedEntities$extractionScriptTree,
                      valueTemplates = rxsStructure$parsedValueTemplates,
                      indent = indent,
                      indentSpaces = indentSpaces,
                      fullWidth = fullWidth,
                      commentCharacter = commentCharacter,
                      fillerCharacter = fillerCharacter,
                      eC = eC,
                      repeatingSuffix = repeatingSuffix,
                      silent=silent);

  ### If this extraction script has any recursing entities, include them
  ### separately
  if (length(rxsStructure$parsedEntities$recursingNodes) > 0) {
    recursingEntitiesChunk <-
      rxs_fg_recursingEntities(rxsStructure$parsedEntities$recursingNodes,
                               valueTemplates = rxsStructure$parsedValueTemplates,
                               indent = indent,
                               indentSpaces = indentSpaces,
                               fullWidth = fullWidth,
                               commentCharacter = commentCharacter,
                               fillerCharacter = fillerCharacter,
                               eC = eC,
                               repeatingSuffix = repeatingSuffix,
                               silent=silent);
    recursingEntitiesChunk <-
      c("```{r rxsChunk-recursingEntities, eval=FALSE, echo=FALSE}",
        unlist(recursingEntitiesChunk),
        "```");
  } else {
    recursingEntitiesChunk <-
      NA;
  }

  if (is.null(yamlMetadata$title)) {
    yamlTitle <- NULL;
  } else {
    yamlTitle <- paste0("title: \"", yamlMetadata$title, "\"");
  }
  if (is.null(yamlMetadata$author)) {
    yamlAuthor <- NULL;
  } else {
    yamlAuthor <- paste0("author: \"", yamlMetadata$author, "\"");
  }
  if (is.null(yamlMetadata$date)) {
    yamlDate <- NULL;
  } else {
    yamlDate <- paste0("date: \"", yamlMetadata$date, "\"");
  }

  yamlHeader <- c("---",
                  yamlTitle,
                  yamlAuthor,
                  yamlDate,
                  "output:",
                  "  html_document:",
                  "    self-contained: yes",
                  "    toc: false",
                  "params:",
                  "  rxsVersion = \"0.1.0\"",
                  "---",
                  "");

  setupChunk <- c("```{r rxsChunk-setup, include=FALSE, messages=FALSE}",
                  "### First load (and perhaps install) userfriendlyscience",
                  "if (!require('userfriendlyscience')) {",
                  "  install.packages('userfriendlyscience');",
                  "  require('userfriendlyscience');",
                  "}",
                  "",
                  "### Other packages",
                  "safeRequire('ufs');              ### To flatten vectors with VecTxtQ in metabefor",
                  "safeRequire('googlesheets');     ### To import data from google sheets in metabefor",
                  "safeRequire('jsonlite');         ### To import a list of country codes in metabefor",
                  "safeRequire('data.tree');        ### To work with data structured in a tree in metabefor",
                  "safeRequire('pander');           ### To print the dataframe with results in a nice way",
                  "safeRequire('devtools');         ### To install metabefor from github repo",
                  "                                 ### ... Which we then do here:",
                  "devtools::install_github('Matherion/metabefor',",
                  "                         quiet=TRUE);",
                  "require('metabefor');            ### ... After which we load it",
                  "",
                  "### Settings",
                  "knitr::opts_chunk$set(echo = FALSE);             ### Suppress R command printing",
                  "knitr::opts_chunk$set(comment=NA);               ### Suppress output prefix",
                  "pander::panderOptions('table.split.table', Inf); ### Disable table splitting",
                  "```");


  printable_eC <-
    paste0(c("eC <- list(", rep(repStr(11), length(eC)-1)),
           paste0(names(eC), "=", '"', unlist(eC), '"'),
           c(character(length(eC)-1), ");"), collapse=",\n");

  valueTemplateCols <-
    attributes(rxsStructure$parsedValueTemplates)$originalColNames;

  printableValueTemplateCols <-
    paste0(c("valueTemplateCols <- list(", rep(repStr(26), length(valueTemplateCols)-1)),
           paste0(names(valueTemplateCols), "=", '"', unlist(valueTemplateCols), '"'),
           c(character(length(valueTemplateCols)-1), ");"), collapse=",\n");


  fieldnameChunk <- c("```{r rxsChunk-fieldnames}",
                      printable_eC,
                      "",
                      printableValueTemplateCols,
                      "```");

  if (!is.null(gs_url)) {
    import_rxsSpecsChunk <- c("```{r rxsSpec-import-chunk}",
                              "",
                              "",
                              "```");
  }

  showExtractedDataChunk <- c("```{r rxsChunk-show-extracted-data, results='asis'}",
                              "print.rxs(study);",
                              "```");

  validationChunk <- c("```{r rxsChunk-validation, results='asis'}",
                       "rxs_validation(study);",
                       "pandoc.header('Validation results', level=1)",
                       "#rxs_validation(study,",
                       "#               rxsStructure = fullResults$rxsStructure);",
                       "if (length(study$validationResults) > 2) {",
                       "  cat(paste0('- ', study$validationResults), sep='\n');",
                       "} else {",
                       "  cat('Validation successful!');",
                       "}",
                       "```");

  res <- c(yamlHeader,
           setupChunk,
           "",
           fieldnameChunk,
           "",
           "```{r rxsChunk, echo=FALSE}",
           scriptChunk,
           "class(study) <- c('rxs', class(study));",
           "```",
           "",
           ifelse(!is.na(recursingEntitiesChunk),
                  c(recursingEntitiesChunk, ""),
                  ""),
           showExtractedDataChunk,
           "",
           validationChunk,
           "");

  return(res);

}
