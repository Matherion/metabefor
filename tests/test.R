########################################################################
### Settings
########################################################################

require("userfriendlyscience")
require("googlesheets");
require('jsonlite');
require('data.tree');
require('here');
#require('metabefor');
### Run all functions in package
updateMetabeforFunctions <- function() {
  invisible(lapply(list.files(here::here("R"),
                              "*.R",
                              full.names=TRUE),
                    source));
}
updateMetabeforFunctions();

testPath <- normalizePath(here::here('../tmp'));


sheetsURL <- paste0("https://docs.google.com/spreadsheets/d/",
                    "14SSzDUeM0H5FuQG808kYV0d61kBAKRE1FhOFEA50CqQ");
valueTemplatesSheet <- "valueTemplates";
entitiesSheet <- "entities";

########################################################################
### Metabefor testing
########################################################################
updateMetabeforFunctions();

fullResults <-
  rxs_fromSpecifications(gs_url = sheetsURL,
                         entitiesFilename = file.path(testPath,
                                                      "test-entities.csv"),
                         valueTemplatesFilename = file.path(testPath,
                                                            "test-valueTemplates.csv"),
                         localBackup = list(entities = file.path(testPath,
                                                                 "test-entities.csv"),
                                            valueTemplates= file.path(testPath,
                                                                      "test-valueTemplates.csv"),
                                            definitions = NULL),
                         outputFile = file.path(testPath, "template.rxs.Rmd"),
                         returnFullObject = TRUE);

#fullResults$rxsStructure$parsedEntities$extractionScriptTree;

