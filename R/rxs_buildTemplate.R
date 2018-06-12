rxs_buildTemplate <- function(rxsStructure,
                              yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                  author = NULL,
                                                  date = format(Sys.time(), '%d %b %Y at %H:%M:%S')),
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

  setupChunk <- c("```{r setup, include=FALSE}",
                  "### First load (and perhaps install) userfriendlyscience",
                  "if (!require('userfriendlyscience')) {",
                  "  install.packages('userfriendlyscience');",
                  "  require('userfriendlyscience');",
                  "}",
                  "",
                  "### The a number of other packages",
                  "safeRequire('googlesheets');     ### To import data from google sheets in metabefor",
                  "safeRequire('jsonlite');         ### To import a list of country codes in metabefor",
                  "safeRequire('data.tree');        ### To work with data structured in a tree in metabefor",
                  "safeRequire('devtools');         ### To install metabefor from github repo",
                  "                                 ### ... Which we then do here:",
                  "devtools::install_github('Matherion/metabefor');",
                  "require('metabefor');",
                  "",
                  "```");

  res <- c(yamlHeader,
           "",
           setupChunk,
           "",
           "```{r rxsChunk, echo=FALSE}",
           scriptChunk,
           "```",
           "",
           validationChunk,
           "");

  return(res);

}
