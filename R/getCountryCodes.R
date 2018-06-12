getCountryCodes <- function() {
  ### https://github.com/apilayer/restcountries
  res <- readLines("https://restcountries.eu/rest/v2/all",
                   warn=FALSE);
  res <- fromJSON(res);
  return(res$alpha2Code);
}
