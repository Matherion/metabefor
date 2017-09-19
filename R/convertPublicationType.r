convertPublicationType <- function(valueOrVector, exportSource="ris",
                                   interpretationDictionary = NULL) {
  
  ### Set default RIS dictionary (based on https://en.wikipedia.org/wiki/RIS_%28file_format%29)
  if (is.null(interpretationDictionary)) {
    if (tolower(exportSource) == 'ris') {
      interpretationDictionary <- matrix(c("BOOK", "book",
                                           "CHAP", "incollection",
                                           "CONF", "inproceedings",
                                           "CPAPER", "inproceedings",
                                           "EBOOK", "book",
                                           "ECHAP", "incollection",
                                           "EDBOOK", "book",
                                           "EJOUR", "journal",
                                           "JFULL", "article",
                                           "JOUR", "article",
                                           "MANSCPT", "article",
                                           "MGZN", "article",
                                           "RPRT", "techreport",
                                           "SER", "book",
                                           "THES", "phdthesis",
                                           "UNPB", "unpublished"
      ),
      ncol=2, byrow=TRUE);
    }
    else {
      stop("Unknown exportSource specified!");
    }
    colnames(interpretationDictionary) <- c('pre', 'post');
    interpretationDictionary <- as.data.frame(interpretationDictionary, stringsAsFactors=FALSE);
  }
  
  ### Replace publication types, use case insensitive matching
  res <- interpretationDictionary$post[match(toupper(valueOrVector), toupper(interpretationDictionary$pre))];
  ### Set unknown types to 'misc'
  res[is.na(res)] <- "misc";
  
  ### Return result
  return(res);
  
}
