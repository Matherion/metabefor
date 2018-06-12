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

  scriptBit <-
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

  setupChunk = c();

  res <- c(yamlHeader,
           setupChunk,
           "```{r rxsChunk, echo=FALSE}",
           scriptBit,
           "```",
           "");

  return(res);

}
