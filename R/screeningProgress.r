screeningProgress <- function(screener1filename, screener2filename,
                              screener1fieldname, screener2fieldname,
                              combinedFieldname, 
                              includedCode = 'incl',
                              excludedCode = 'excl',
                              silent=FALSE,
                              identifier = 'bibtexkey') {
  
  screener1fieldname <- tolower(screener1fieldname);
  screener2fieldname <- tolower(screener2fieldname);
  
  ### Read screener files after first screening round
  combined <- importBibtex(screener1filename);
  screener2 <- importBibtex(screener2filename);
  
  ### Add field from screener 2
  combined$records[[screener2fieldname]] <- screener2$records[[screener2fieldname]];
  
  ### Create combined judgement field
  combined$records[[combinedFieldname]] <-
    ifelse((tolower(combined$records[, tolower(names(combined$records)) == screener1fieldname]) == tolower(includedCode)) |
             (tolower(combined$records[, tolower(names(combined$records)) == screener2fieldname]) == tolower(includedCode)),
           includedCode, excludedCode);
  
  ### Verify
  if (!silent) {
    print(na.omit(data.frame(bibtex = combined$records[[identifier]],
                             combined = combined$records[[combinedFieldname]],
                             screener1 = combined$records[[screener1fieldname]],
                             screener2 = combined$records[[screener2fieldname]])));
  }
  
  ### Copy dataframe to $output subobject
  combined$output <- list(records = combined$records);
  
  ### Return result
  return(combined);
  
}
