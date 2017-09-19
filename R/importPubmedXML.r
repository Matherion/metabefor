importPubmedXML <- function(filename, encoding="UTF-8", ...) {
  
  # Read the text from the file
  xmlText <- paste(readLines(filename), "\n", collapse="")
  xmlTree <- xmlTreeParse(xmlText, asText=TRUE, useInternalNodes=TRUE);
  xmlList <- xmlToList(xmlTree);
  
  ### Generate object to return
  res <- list(input = list(filename = filename,
                           encoding = encoding),
              intermediate = list(),
              output = list(xmlList = xmlList));
    
  class(res) <- 'sysrev reference XML list';
  return(res);
  
}

extractPubmedFields <- function(pubmedList) {
  ### Create all fields and set to NA
  authors <- year <- title <- journal <- volume <- issue <- abstract <- NA;
  if ("MedlineCitation" %in% names(pubmedList)) {
    if ("Article" %in% names(pubmedList$MedlineCitation)) {
      if ("Author" %IN% names(pubmedList$MedlineCitation$Article$AuthorList)) {
         authors <- buildAuthorsFromPubMedList(pubmedList$MedlineCitation$Article$AuthorList);
      }
      if ("ArticleTitle" %in% names(pubmedList$MedlineCitation$Article)) {
        title <- pubmedList$MedlineCitation$Article$ArticleTitle;
      }
      if ("Abstract" %IN% names(pubmedList$MedlineCitation$Article)) {
        abstract <- buildAbstractFromPubMedList(pubmedList$MedlineCitation$Article$Abstract);
      }
    }
    if ("Journal" %in% names(pubmedList$MedlineCitation$Article)) {
      if ("Title" %in% names(pubmedList$MedlineCitation$Article$Journal)) {
        journal <- pubmedList$MedlineCitation$Article$Journal$Title;
      }
      if ("JournalIssue" %in% names(pubmedList$MedlineCitation$Article$Journal)) {
        if ("PubDate.Year" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue)) {
          year <- pubmedList$MedlineCitation$Article$Journal$JournalIssue$PubDate.Year;
        }
        else if ("PubDate" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue)) {
          if ("Year" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue$PubDate)) {
            year <- pubmedList$MedlineCitation$Article$Journal$JournalIssue$PubDate['Year'];
          }
          else if ("MedlineDate" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue$PubDate)) {
            year <- pubmedList$MedlineCitation$Article$Journal$JournalIssue$PubDate['MedlineDate'];
          }
        }
        if ("Volume" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue)) {
          volume <- pubmedList$MedlineCitation$Article$Journal$JournalIssue$Volume;
        }
        if ("Issue" %in% names(pubmedList$MedlineCitation$Article$Journal$JournalIssue)) {
          issue <- pubmedList$MedlineCitation$Article$Journal$JournalIssue$Issue;
        }
      }      
    }
  }
  #print(title);
  return(data.frame(authors, year, title, journal, volume, issue, abstract));
}

buildAuthorsFromPubMedList <- function(authorList) {
  return(paste(laply(authorList[names(authorList)=="Author"], function(x) {
    return( ifelse(!is.na(x['LastName']), paste(x['ForeName'], x['LastName']), "" ));
  }), collapse=" and "));
}

buildAbstractFromPubMedList <- function(abstractList) {
  return(paste(laply(abstractList[names(abstractList)=="AbstractText"], function(x) {
    if (is.character(x)) {
      return(as.character(paste(x, collapse=" ")));
    }
    else if (!is.na(x['text'])) {
      return(as.character(x['text']));
    }
    else {
      return(character(1));
    }
  }), collapse=" "));
}


