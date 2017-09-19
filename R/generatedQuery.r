generatedQuery <- function(query, fields='ti', exclude=NULL) {
  res <- list(input = list(query = query),
              intermediate = list(),
              output = list());
  
  if (fields=='ti') {
    pubMedFields <- ' [TI]';
    ebscoHostFields <- 'TI ';
    ovidFields <- '.ti.';
  }

  if (fields=='tiab') {
    pubMedFields <- ' [TIAB]';
    ebscoHostFields <- 'TI ';
    ovidFields <- '.ti.ab';
  }
  
  if (fields=='tx') {
    pubMedFields <- ' [Text Word]';
    ebscoHostFields <- 'TX ';
    ovidFields <- '.tw.';
  }
  
  ### Generate PubMed Query
  ### First combine terms in each term set using OR
  res$intermediate$pubmed <- lapply(query, FUN = function(termSet) {
    paste(paste0('(', termSet, pubMedFields, ')'), collapse=" OR ");
  });
  ### Then combine termSets using AND into query
  res$output$pubmed <- paste0("((", paste0(res$intermediate$pubmed, collapse = ") AND ("), "))");
  
  ### Generate Ebscohost Query
  ### First combine terms in each term set using OR
  res$intermediate$ebscohost <- lapply(query, FUN = function(termSet) {
    paste(paste0('"', termSet, '"'), collapse=" OR ");
  });
  ### Then combine termSets using AND into query
  res$output$ebscohost <- paste0(ebscoHostFields,
                                 "((",
                                 paste0(res$intermediate$ebscohost, collapse = ") AND ("),
                                 "))");
  
  ### Generate Ovid Query
  ### First combine terms in each term set using OR
  res$intermediate$ovid <- lapply(query, FUN = function(termSet) {
    paste(paste0('"', termSet, '"'), collapse=" OR ");
  });
  ### Then combine termSets using AND into query
  basicQuery <- paste0("((", paste0(res$intermediate$ovid, collapse = ") AND ("), "))");
  if (!is.null(exclude)) {
    exclusionBit <- paste0(" NOT (", paste0(exclude, collapse=" OR "), ")");
  } else {
    exclusionBit <- "";
  }
  res$output$ovid <- paste0("(", basicQuery, exclusionBit, ")", ovidFields);
  
  ### Add the exclusion terms to the first two queries
  if (!is.null(exclude)) {
    res$output$pubmed <- paste0(res$output$pubmed, " NOT (",
                                paste0(exclude, collapse=" OR "), ")");
    res$output$ebscohost <- paste0(res$output$ebscohost, " NOT (",
                                   paste0(exclude, collapse=" OR "), ")");
  }
  
  class(res) <- 'generatedQuery';
  return(res);
  
}

print.generatedQuery <- function(x, ...) {
  cat(paste0("### PUBMED QUERY:\n", x$output$pubmed, "\n\n"));
  cat(paste0("### EBSCOHOST QUERY:\n", x$output$ebscohost, "\n\n"));
  cat(paste0("### OVID QUERY:\n", x$output$ovid, "\n\n"));
  cat(paste0("NOTE: export the results as .RIS files, called 'MEDLINE' in PubMed."));
}
