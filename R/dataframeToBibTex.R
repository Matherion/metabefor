dataframeToBibTex <- function(dataframe, screeningType) {
  if (!is.data.frame(dataframe)) {
    stop("dataframeToBibTex requires a dataframe, but instead, an object of class '",
         class(dataframe), "' is provided!");
  }
  ### First convert each cell in the data frame to
  ### a combination of fieldname and value (but skip the 'type')
  res <- apply(dataframe, 1,
               function(x, screeningType=screeningType) {
                 ### Store bibtexkey
                 if(is.null(x['bibtexkey'])) {
                   bibtexkey <- ""
                 }
                 else {
                   bibtexkey <- x['bibtexkey'];
                   ### Also remove it so we don't store it in the reference
                   x <- x[names(x) != 'bibtexkey'];
                 }
                 ### Store reference type
                 if (!is.null(screeningType)) {
                   type <- screeningType;
                 }
                 else {
                   if(is.null(x['type'])) {
                     type <- "misc"
                   }
                   else {
                     type <- x['type'];
                     ### Also remove it so we don't store it in the reference
                     x <- x[names(x) != 'type'];
                   }
                 }
                 ### Remove NA values
                 x <- x[!is.na(x)];
                 ### Escape pound signs and curly braces
                 x <- gsub("(#|\\{|\\})", "\\\\\\1", x);
                 ### Generate and return result
                 return(c(paste0("@", type, "{", bibtexkey, ","),
                          paste0("  ", names(x), " = {", x, "},"),
                          "}", ""));
               }, screeningType=screeningType);
  return(unlist(res));
}
