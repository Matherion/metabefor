query_toInterfaceLang <- function(queryObject,
                                  fields='title',
                                  exclude=NULL) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  query <- queryObject$Get(function(node)
      return(node$Get("name",
                      filterFun=isLeaf)),
    filterFun=function(n)
      return(!n$isLeaf && !n$isRoot));

  searchInTitle <- grepl('title', fields, ignore.case=TRUE);
  searchInAbstract <- grepl('abstract', fields, ignore.case=TRUE);
  searchInTextWords <- grepl('text words', fields, ignore.case=TRUE);

  if (searchInTitle && !searchInAbstract && !searchInTextWords) {
    pubMedFields <- ' [TI]';
    ebscoHostFields <- 'TI ';
    ovidFields <- '.ti';
  } else if (searchInTitle && searchInAbstract && !searchInTextWords) {
    pubMedFields <- ' [TIAB]';
    ebscoHostFields <- c('TI ', 'AB ');
    ovidFields <- '.ti,ab';
  } else if (!searchInTitle && !searchInAbstract && searchInTextWords) {
    pubMedFields <- ' [Text Word]';
    ebscoHostFields <- 'TX ';
    ovidFields <- '.tw';
  } else {
    stop("Unknown field of combination of fields specified!");
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
    return(paste0("(", paste(paste0('"', termSet, '"'), collapse=" OR "),
                  ")"));
  });

  ### Then, if looking in both title and abstract, double the
  ### termsets with each field specifyer
  # if (length(ebscoHostFields) > 1) {
  #   res$intermediate$ebscohost <-
  #     lapply(ebscoHostFields, FUN = function(x, termSet=termSet) {
  #       return(paste0(x, " "
  #     });
  # }

  res$intermediate$ebscohost_halfway <- c();
  for (currentTermSet in res$intermediate$ebscohost) {
    currentTermSetCollection <- c();
    for (currentField in ebscoHostFields) {
      currentTermSetCollection <-
        append(currentTermSetCollection,
               paste(currentField, currentTermSet));
    }
    res$intermediate$ebscohost_halfway <-
      append(res$intermediate$ebscohost_halfway,
             paste0(paste0("(",
                           currentTermSetCollection,
                           ")"),
                    collapse=" OR "));
  }

  res$intermediate$ebscohost_halfway <-
    paste0("(",
           res$intermediate$ebscohost_halfway,
           ")");

  res$output$ebscohost <-
    paste(res$intermediate$ebscohost_halfway,
          collapse=" AND ");

  # ### Create all combinations of fields and
  # ### term sets
  # res$intermediate$ebscohost_halfway <-
  #   purrr::cross2(ebscoHostFields,
  #                 res$intermediate$ebscohost);
  #
  # ### Name the result
  # names(res$intermediate$ebscohost_halfway) <-
  #   res$intermediate$ebscohost_halfway %>%
  #   map(.f=function(x) return(x[[2]]));
  #
  # ### Paste the field names and term sets together using map,
  # ### then process further by list name to combine
  # ### with OR
  # res$intermediate$ebscohost_halfway <-
  #   res$intermediate$ebscohost_halfway %>%
  #   purrr::map(paste, collapse=" ") %>%
  #   rbind %>%
  #   t;
  #
  # ### Get the names of the term sets
  # termSetNames <-
  #   row.names(res$intermediate$ebscohost_halfway);
  #
  # ### Convert to dataframe
  # res$intermediate$ebscohost_halfway <-
  #   as.data.frame(res$intermediate$ebscohost_halfway,
  #                 stringsAsFactors=FALSE);
  #
  # names(res$intermediate$ebscohost_halfway) <-
  #   'fieldAndTermSet';
  #
  # ### Add row names as variable and remove row names
  # res$intermediate$ebscohost_halfway$termSet <-
  #   termSetNames;
  # row.names(res$intermediate$ebscohost_halfway) <- NULL;
  #
  # ### Add parentheses around field/termset combination
  # res$intermediate$ebscohost_halfway$fieldAndTermSet <-
  #   paste0("(", res$intermediate$ebscohost_halfway$fieldAndTermSet, ")");
  #
  # res$output$ebscohost <-
  #   res$intermediate$ebscohost_halfway %>%
  #   dplyr::group_by(termSet) %>%
  #   dplyr::select(fieldAndTermSet) %>%
  #   paste(collapse=" AND ")
  #
  # ### Then combine termSets using AND into query
  # res$output$ebscohost <- paste0(ebscoHostFields,
  #                                "((",
  #                                paste0(res$intermediate$ebscohost, collapse = ") AND ("),
  #                                "))");

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

  class(res) <- 'query_toInterfaceLang';
  return(res);
}

print.query_toInterfaceLang <- function(x, header = "3", ...) {
  cat(paste0(repStr("#", header), " PUBMED QUERY:\n", x$output$pubmed, "\n\n"));
  cat(paste0(repStr("#", header), " EBSCOHOST QUERY:\n", x$output$ebscohost, "\n\n"));
  cat(paste0(repStr("#", header), " OVID QUERY:\n", x$output$ovid, "\n\n"));
  cat(paste0("NOTE: export the results as .RIS files, called 'MEDLINE' in PubMed."));
}
