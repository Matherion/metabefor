generateBibtexkeys <- function(dataframe, authorMinLength = 2) {
  if (!is.data.frame(dataframe)) {
    stop("generateBibtexkeys requires a dataframe, but instead, an object of class '", 
         class(dataframe), "' is provided!");
  }
  
  ### Convert all field names to lower case
  oldFieldNames <- names(dataframe);
  names(dataframe) <- tolower(names(dataframe));
  
  ### Get author names, or a words from the title,
  ### for the first part of the bibtex keys
  if ('author' %in% names(dataframe)) {
    firstPartVector <- dataframe[, 'author'];
    firstPartVector <- getFirstAuthor(firstPartVector, minLength = authorMinLength);
  }
  else if ('authors' %in% names(dataframe)) {
    firstPartVector <- dataframe[, 'authors'];
    firstPartVector <- getFirstAuthor(firstPartVector, minLength = authorMinLength);
  }
  else if ('title' %in% names(dataframe)) {
    firstPartVector <- dataframe[, 'title'];
    ### Get the first long word
    firstPartVector <- unlist(lapply(firstPartVector, getFirstLongWord));
  }
  
  ### Get the year, or a word from the title, or from
  ### the abstract, as the second part
  if ('year' %in% names(dataframe)) {
    secondPartVector <- dataframe[, 'year'];
    secondPartVector <- unlist(lapply(secondPartVector, getFirstLongWord, wordLengthCeiling=4));
  }
  else if (('author' %in% names(dataframe)) || ('authors' %in% names(dataframe))) {
    ### If we don't have a year, but we have an author field, we can
    ### use the title field here
    secondPartVector <- dataframe[, 'title'];
    secondPartVector <- unlist(lapply(secondPartVector, getFirstLongWord, wordLengthCeiling=8));
  }
  else if ('abstract' %in% names(dataframe)) {
    ### Use the first word from the abstract field
    secondPartVector <- dataframe[, 'abstract'];
    secondPartVector <- unlist(lapply(secondPartVector, getFirstLongWord, wordLengthCeiling=8));
  }
  else {
    ### Abort
    stop("Specified dataframe does not have an author field and at least a year, title, ",
         "or abstract field! Not able to construct BibTeX keys like this.");
  }
  
  ### Combine the vectors
  newKeys <- originalKeys <- paste0(trim(firstPartVector), trim(secondPartVector));
  
  ### Unduplicate new keys 'internally'
  currentLetter <- 1;
  while (TRUE %in% duplicated(newKeys)) {
    newKeys[duplicated(newKeys)] <-
      paste0(originalKeys[duplicated(newKeys)], letters[currentLetter]);
    replaceLastCharacter <- duplicated(newKeys);
    currentLetter <- currentLetter + 1;
  }

  ### Restore original fieldnames (i.e. in whichever case)
  names(dataframe) <- oldFieldNames;
  
  if (!('bibtexkey' %in% names(dataframe))) {
    dataframe$bibtexkey <- newKeys;
  }
  else {
    
    allKeys <- c(unique(dataframe$bibtexkey), newKeys);
    
    ### Unduplicate new keys 'externally'
    #     currentLetter <- 1;
    #     while (TRUE %in% (newKeys %in% allKeys)) {
    #       newKeys[newKeys %in% allKeys] <-
    #         paste0(originalKeys[newKeys %in% allKeys], letters[currentLetter]);
    #       allKeys <- c(unique(dataframe$bibtexkey), newKeys);
    #       currentLetter <- currentLetter + 1;
    #     }
    
    
    ### Replace empty bibtexkeys with newly generated keys
    dataframe$bibtexkey <- ifelse(is.na(dataframe$bibtexkey),
                                  newKeys,
                                  dataframe$bibtexkey);
  }
  
  ### Return result
  return(dataframe);
}

getFirstAuthor <- function(textString, minLength = 2, split="[^a-zA-Z0-9]") {
  ### Split by all non-alphanumeric characters, extract length
  ### of every fragment, compare to minLength, remove all
  ### fragments that are too short, extract the first
  ### fragment of what remains, and and unlist these back
  ### to a character vector
  textString <- unlist(lapply(strsplit(textString, split), function(x) {
    wordLengths <- nchar(x);
    return(x[wordLengths>=minLength][1]);
  }))
  return(textString);
}

getFirstLongWord <- function(textString, wordLengthCeiling = 15, split="[^a-zA-Z0-9]") {
  words <- unlist(strsplit(textString, split));
  wordLengths <- ifelse(nchar(words) <= wordLengthCeiling, nchar(words), NA);
  selectedWords <- ifelse(wordLengths == max(wordLengths, na.rm=TRUE), TRUE, FALSE);
  return(words[selectedWords][1]);
}
