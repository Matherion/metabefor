interpretRISlike <- function(referenceList, exportSource="ris", interpretationDictionary=NULL,
                             progress = TRUE) {
  
  ### Check class of first argument
  if (!(class(referenceList) == "sysrev reference list")) {
    stop("interpretRISlike can only interpret sysrev reference list objects; the class of the object ",
         "you provided is '", class(referenceList), "'.");
  }
  
  ### Set default RIS dictionary (based on https://en.wikipedia.org/wiki/RIS_%28file_format%29)
  if (is.null(interpretationDictionary)) {
    if (exportSource == 'ris') {
      interpretationDictionary <- matrix(c("TY",       "type",
                                           
                                           "AU",       "author",
                                           "A1",       "author",
                                           "A2",       "author.secondary",
                                           "A3",       "author.tertairy",
                                           "A4",       "author.subsidiary",
                                           
                                           "AB",       "abstract",
                                           "N2",       "abstract",
                                           
                                           "AD",       "address.author",
                                           "AN",       "number.accession",
                                           "C(\\d)",   "custom.\\1",
                                           "CA",       "caption",
                                           "CN",       "number.call",
                                           "CY",       "place.published",
                                           "DA",       "date",
                                           "DB",       "database.name",
                                           "DO",       "doi",
                                           "DP",       "database.provider",
                                           "EP",       "page.end",
                                           "ET",       "edition",
                                           "IS",       "issue",
                                           "ID",       "id.record",
                                           
                                           "J2",       "title.alternate",
                                           "JF",       "journal",
                                           "JA",       "journal.abbreviated",
                                           
                                           "K(\\d|W)", "keywords",
                                           
                                           "L1",       "attached.file.local",
                                           "L2",       "url.ovid",
                                           "L4",       "attached.figure.local",
                                           "LA",       "language",
                                           "LB",       "label",
                                           "M1",       "number",
                                           "M3",       "work.type",
                                           "N([013456789]|O)", "notes",
                                           "NV",       "volumes.numberof",
                                           "OP",       "publication.original",
                                           "PB",       "publisher",
                                           
                                           "(PY|Y1)",  "year",
                                           
                                           "RI",       "item.reviewed",
                                           "RN",       "notes.research",
                                           "RP",       "edition.reprint",
                                           "SE",       "section",
                                           "SN",       "number.serial",
                                           "SP",       "page.start",
                                           
                                           "ST",       "title.short",
                                           "TI",       "title",
                                           "T1",       "title",
                                           "T2",       "title.secundary",
                                           "T3",       "title.tertairy",
                                           "TT",       "title.translated",
                                           
                                           "TA",       "author.translated",
                                           "UR",       "url",
                                           
                                           "VL",       "volume",
                                           
                                           "Y2",       "date.accessed"
      ),
      ncol=2, byrow=TRUE);
    }
    else if (exportSource == 'medline') {
      interpretationDictionary <- matrix(c("PMID",     "id.pubmed",
                                           
                                           "OWN",      "owner",
                                           "STAT",     "status",
                                           "DA",       "date",
                                           "IS",       "number.serial",
                                           "VI",       "volume",
                                           "IP",       "issue",
                                           "DP",       "year",
                                           "TI",       "title",
                                           "PG",       "pages",
                                           "AB",       "abstract",
                                           "FAU",      "author.full",
                                           "AU",       "author",
                                           "LA",       "language",
                                           "PT",       "type",
                                           "PL",       "land.published",
                                           "TA",       "journal.abbreviated",
                                           "JT",       "journal",
                                           "JID",      "id.journal",
                                           "SO",       "source.ref",
                                           "LID",      "doi",
                                           "AD",       "address"
      ),
      ncol=2, byrow=TRUE);    }
    else {
      stop("Unknown exportSource specified!");
    }
    colnames(interpretationDictionary) <- c('tag', 'field');
    interpretationDictionary <- as.data.frame(interpretationDictionary, stringsAsFactors=FALSE);    
  }

  ### Generate object to return results
  res <- list(input = list(referenceList = referenceList),
              output = list());

  cat0("\nInterpreting references...\n\n");
  
  if (progress) pBar <- txtProgressBar(style=3,
                                       max = length(referenceList));
  
  ### Loop through records, and convert each tag to the field name
  ### according to the interpretation dictionary
  res$output$recordList <- list();
  for (currentReference in seq_along(referenceList)) {
    if (progress) setTxtProgressBar(pBar, currentReference);
    res$output$recordList[[currentReference]] <- list();
    for (currentField in seq_along(referenceList[[currentReference]])) {
      ### Match regular expressions, store match
      indexInDictionary <- unlist(lapply(interpretationDictionary$tag,
                                         FUN=grepl,
                                         names(referenceList[[currentReference]])[currentField]));
      ### Get index of match (note that there should always be
      ### only one match - otherwise, we just take the first one)
      indexInDictionary <- match(TRUE, indexInDictionary);
      ### Store new name of current field
      if (!is.na(indexInDictionary)) {
        ### Use regex replacement to generate real field name
        newFieldName <- sub(interpretationDictionary$tag[indexInDictionary],
                            interpretationDictionary$field[indexInDictionary],
                            names(referenceList[[currentReference]])[currentField]);
      }
      else {
        ### Just store original tag from RIS file
        newFieldName <- names(referenceList[[currentReference]])[currentField];
      }
      res$output$recordList[[currentReference]][[newFieldName]] <-
        referenceList[[currentReference]][[currentField]];
      if(tolower(newFieldName) == "type") {
        res$output$recordList[[currentReference]][[newFieldName]] <-
          convertPublicationType(res$output$recordList[[currentReference]][[newFieldName]]);
      }
    }
  }
  
  class(res) <- "sysrev interpreted reference list";
  return(res);
}
