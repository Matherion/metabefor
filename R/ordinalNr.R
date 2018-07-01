ordinalNr <- function(x) {
  return(case_when((x == 1) ~ "1st",
                   (x == 2) ~ "2nd",
                   (x == 3) ~ "3rd",
                   TRUE ~ paste0(x, "th")));
}
