query_full <- function(inclusion,
                       exclusion = NULL,
                       queryName = "query") {

  if (is.null(exclusion)) {
    return(inclusion);
  }

  resNode <- data.tree::Node$new(queryName %||% "query");
  resNode$operator <- "NOT";

  resNode$AddChildNode(inclusion);

  if (class(exclusion) == "query_conceptTerms") {
    childName <- attr(exclusion, "conceptName");
    resNode$AddChild(childName);
    resNode[[childName]]$object <-
      exclusion;
    resNode[[childName]]$operator <-
      attr(exclusion, "operator");
    for (j in seq_along(exclusion)) {
      resNode[[childName]]$AddChild(exclusion[j]);
    }
  } else if (class(exclusion) == "query_requiredConcepts") {
    AddChildNode(exclusion);
  } else {
    stop("Argument `exclusion` has an invalid class!");
  }

  SetGraphStyle(resNode, rankdir = "LR");
  SetEdgeStyle(resNode,
               arrowhead = "vee",
               color = "#000000",
               style="solid",
               penwidth = 2);
  SetNodeStyle(resNode,
               style = "filled,rounded",
               shape = "box",
               fillcolor = "#DDDDDD",
               fontname = "helvetica");

  resNode$Do(function(node)
    SetEdgeStyle(node,
                 style = case_when(node$parent$operator=="OR" ~ "dotted",
                                   node$parent$operator=="AND" ~ "solid",
                                   node$parent$operator=="NOT" ~ "dashed",
                                   TRUE ~ "solid")),
    traversal="level");


  attr(resNode, "queryName") <- queryName;
  class(resNode) <- c('query_full', class(resNode));
  return(resNode);
}

print.query_full <- function(x, ...) {
  x <- data.tree::Clone(x);
  class(x) <- setdiff(class(x), 'query_full');
  print(x);
  invisible(x);
}
