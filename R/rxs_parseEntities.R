rxs_parseEntities <- function(entities,
                              eC = entityColNames(),
                              rootName = 'study') {

  ### Prepare dataframe with entities for conversion to a tree
  dataFrameNetwork <-
    as.data.frame(entities[!is.na(entities[[eC$identifierCol]]),
                           unique(c(eC$identifierCol,
                                    eC$parentCol, names(entities)))]);

  if (getOption("metabefor.debug", FALSE)) {
    ufs::cat0("\nrxs_parseEntities read an entity spreadsheet with the following columns: ",
              ufs::vecTxtQ(names(dataFrameNetwork)), ".\n");
  }

  ### Add a root entity for the entities without one
  dataFrameNetwork[[eC$parentCol]][is.na(dataFrameNetwork[[eC$parentCol]])] <-
    rootName;

  ### Check for nonexistent parents
  nonExistentParents <-
    !(dataFrameNetwork[[eC$parentCol]] %in% c(rootName, dataFrameNetwork[[eC$identifierCol]]));
  if (any(nonExistentParents)) {
    stop("The items with the following identifiers have a parent that ",
         "cannot be found in the list of parents:\n\n",
         paste0(paste0("  - '",
                       dataFrameNetwork[[eC$identifierCol]][nonExistentParents],
                       "' with parent '",
                       dataFrameNetwork$Parent[nonExistentParents],
                       "' on line ",
                       which(entities[[eC$identifierCol]] %in%
                               entities[[eC$identifierCol]][nonExistentParents])),
                collapse=";\n"),
         ")!");
  }

  ### Convert to tree
  extractionScriptTree <- data.tree::FromDataFrameNetwork(dataFrameNetwork);

  ### Check for unique names
  if (!data.tree::AreNamesUnique(extractionScriptTree)) {
    warning(paste0("Not all identifiers in the extraction script ",
                   "tree are unique! Duplicated elements: ",
                   vecTxtQ(dataFrameNetwork[[eC$identifierCol]][duplicated(dataFrameNetwork[[eC$identifierCol]])]),
                   ". This may cause problems - it is advisable ",
                   "to make sure identifiers are unique."));
  }


  ### Retrieve all recursing node definitions and place them
  ### in a separate list. We do this first for the recursing
  ### nodes, then for the recurring nodes, because all recursing
  ### nodes are recurring nodes.
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

  ### Retrieve all recurring node definitions and place them
  ### in a separate list
  recurringNodes <-
    Traverse(extractionScriptTree,
             traversal="level",
             filterFun=function(node) {
               return(isRecurringEntityDefinition(node,
                                                  recurringColName=eC$recurringCol));
             });
  ### Remove all recursing node definitions from the extraction
  ### script tree
  numberOfRecurringEntities <-
    Prune(extractionScriptTree,
          pruneFun=function(node) {
            return(!isRecurringEntityDefinition(node,
                                                recurringColName=eC$recurringCol));
          });
  ### Name the recurring entities
  names(recurringNodes) <-
    sapply(recurringNodes, function(x) return(x$name));

  ### Add all recurring node definitions in the tree
  ### where they are included
  extractionScriptTree$Do(function(node,
                                   recurringColName=eC$recurringCol,
                                   recurrNodes=recurringNodes) {

                            ### Check which recursive node to add
                            nodeToInclude <- node[[recurringColName]];
                            ### Add each child
                            for (currentChild in recurrNodes[[nodeToInclude]]$children) {
                              node$AddChildNode(Clone(currentChild));
                            }

                          },
                          filterFun = function(node) {
                            return(isRecurringEntityInclusion(node,
                                                              recurringColName=eC$recurringCol));
                          });
  ### Add all recursing node definitions in the tree
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
              recurringNodes=recurringNodes,
              recursingNodes=recursingNodes);

  attr(res, "numberOfRecurringEntities") <- numberOfRecurringEntities;
  attr(res, "numberOfRecursingEntities") <- numberOfRecursingEntities;

  class(res) <- "parsedEntities";

  return(res);

}



