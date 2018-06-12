rxs_parseEntities <- function(entities,
                              eC = entityColNames(),
                              rootName = 'study') {

  ### Prepare dataframe with entities for conversion to a tree
  dataFrameNetwork <-
    as.data.frame(entities[!is.na(entities[[eC$identifierCol]]),
                           unique(c(eC$identifierCol,
                                    eC$parentCol, names(entities)))]);

  ### Add a root entity for the entities without one
  dataFrameNetwork[[eC$parentCol]][is.na(dataFrameNetwork[[eC$parentCol]])] <-
    rootName;

  ### Check for nonexistent parents
  nonExistentParents <-
    !(dataFrameNetwork[[eC$parentCol]] %in% c(rootName, dataFrameNetwork[[eC$identifierCol]]));
  if (any(nonExistentParents)) {
    stop("The items with the following identifiers have a parent that ",
         "cannot be found in the list of parents: ",
         paste0(paste0("'",
                       dataFrameNetwork[[eC$identifierCol]][nonExistentParents],
                       "' with parent '",
                       dataFrameNetwork$Parent[nonExistentParents],
                       "' on line ",
                       which(entities[[eC$identifierCol]] %in%
                               entities[[eC$identifierCol]][nonExistentParents])),
                collapse=", "),
         ")!");
  }

  ### Convert to tree
  extractionScriptTree <- FromDataFrameNetwork(dataFrameNetwork);

  ### Check for unique names
  if (!data.tree::AreNamesUnique(extractionScriptTree)) {
    warning(paste0("Not all identifiers in the extraction script ",
                   "tree are unique! Duplicated elements: ",
                   vecTxtQ(dataFrameNetwork[[eC$identifierCol]][duplicated(dataFrameNetwork[[eC$identifierCol]])]),
                   ". This may cause problems - it is advisable ",
                   "to make sure identifiers are unique."));
  }

  ### Retrieve all recursing node definitions and place them
  ### in a separate list
  recursingNodes <-
    Traverse(extractionScriptTree,
             traversal="level",
             filterFun=function(node) {
               return(isRecursingEntityDefinition(node,
                                                  recursingColName=eC$recursingCol));
             });

  ### Remove all recursing node definitions from the extraction
  ### script tree
  numberOfRecursingEntities <-
    Prune(extractionScriptTree,
          pruneFun=function(node) {
            return(!isRecursingEntityDefinition(node,
                                                recursingColName=eC$recursingCol));
          });

  ### Name the recursing entities
  names(recursingNodes) <-
    sapply(recursingNodes, function(x) return(x$name));

  ### Add all recursive node definitions in the tree
  ### where they are included
  extractionScriptTree$Do(function(node,
                                   recursingColName=eC$recursingCol,
                                   recursNodes=recursingNodes) {

                            ### Check which recursive node to add
                            nodeToInclude <- node[[recursingColName]];
                            ### Add each child
                            for (currentChild in recursNodes[[nodeToInclude]]$children) {
                              node$AddChildNode(Clone(currentChild));
                            }

                          },
                          filterFun = function(node) {
                            return(isRecursingEntityInclusion(node,
                                                              recursingColName=eC$recursingCol));
                          });

  res <- list(extractionScriptTree=extractionScriptTree,
              recursingNodes=recursingNodes);

  attr(res, "numberOfRecursingEntities") <- numberOfRecursingEntities;

  class(res) <- "parsedEntities";

  return(res);

}



