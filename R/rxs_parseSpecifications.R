rxs_parseSpecifications <- function(entities,
                                    valueTemplates,
                                    definitions = NULL,
                                    eC = entityColNames(),
                                    valueTemplateCols = valueTemplateColNames(),
                                    rootName = 'study') {

  res <- list(parsedEntities = rxs_parseEntities(entities = entities,
                                                 eC = eC,
                                                 rootName = rootName),
              parsedValueTemplates = rxs_parseValueTemplates(valueTemplateDataframe=valueTemplates,
                                                             valueTemplateCols = valueTemplateCols));

  class(res) <- "rxsStructure";

  return(res);

}



