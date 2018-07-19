entityColNames <- function(...) {
  res <- c(titleCol = "title",
           descriptionCol = "description",
           identifierCol = "identifier",
           valueTemplateCol = "valueTemplate",
           validValuesCol = "validValues",
           defaultCol = "default",
           parentCol = "parent",
           entityRefCol = "entityRef",
           fieldRefCol = "fieldRef",
           ownerCol = "owner",
           listCol = "list",
           collapsingCol = "collapsing",
           repeatingCol = "repeating",
           recurringCol = "recurring",
           recursingCol = "recursing",
           identifyingCol = "identifying");
  tweaks <- unlist(list(...));
  presentElements <- names(tweaks) %in% names(res);
  if (!all(presentElements)) {
    warning(paste0("Some column names (",
                   vecTxtQ(names(tweaks)[!presentElements]),
                   ") are invalid! Ignoring these."));
    tweaks <- tweaks[presentElements];
  }
  res[names(tweaks)] <- tweaks;
  res <- as.list(res);
  return(res);
};
