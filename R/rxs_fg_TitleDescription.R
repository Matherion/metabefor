rxs_fg_TitleDescription <- function(title,
                                    description,
                                    level = 0,
                                    indent = TRUE,
                                    indentSpaces = 2,
                                    fullWidth = 80,
                                    commentCharacter = "#",
                                    fillerCharacter = "#") {

  lV <- rxs_fg_layoutVars(level = level,
                          indent = indent,
                          indentSpaces = indentSpaces,
                          fullWidth = fullWidth,
                          commentCharacter = commentCharacter,
                          fillerCharacter = fillerCharacter);

  res <- lV$lineFiller;

  if (!is.null(title)) {
    res <- c(res,
             lV$commentPrefix,
             strwrap(toupper(title), width=lV$commentWidth, prefix=lV$commentPrefix));
  }

  if (!is.null(description)) {
    res <- c(res,
             c(lV$commentPrefix,
               strwrap(description, width=lV$commentWidth, prefix=lV$commentPrefix)));
  }

  ### Construct the character vector to return
  if (!is.null(title) || !is.null(description)) {
    res <- c(res,
             lV$commentPrefix,
             lV$lineFiller);
  }

  return(res);

}
