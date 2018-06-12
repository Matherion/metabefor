rxs_fg_layoutVars <- function(level=1,
                              indent = TRUE,
                              indentSpaces = 2,
                              fullWidth = 80,
                              commentCharacter = "#",
                              fillerCharacter = "#") {

  res <- list();

  level <- level-1;

  if (indent) {
    res$indentSpaces <- repStr(level * indentSpaces);
    res$fullWidth <- fullWidth - nchar(res$indentSpaces);
  } else {
    res$indentSpaces <- "";
    res$fullWidth <- fullWidth;
  }
  res$lineFiller <- paste0(res$indentSpaces,
                           repStr(fillerCharacter,
                                  res$fullWidth));
  res$commentWidth <- res$fullWidth - 4;
  res$commentPrefix <- paste0(res$indentSpaces,
                              commentCharacter,
                              repStr(fillerCharacter, 2),
                              " ");
  res$valuePrefix <- paste0(res$indentSpaces, repStr(4));

  return(res);

}
