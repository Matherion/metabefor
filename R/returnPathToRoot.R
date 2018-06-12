returnPathToRoot <- function(node,
                             considerRoot = "") {
  res <- paste0(rev(unlist(sapply(Traverse(node, traversal='ancestor'),
                                  function(x) return(x$name)),
                           recursive=TRUE)), collapse="$");
  res <- gsub(paste0("^.*\\$", considerRoot, "$"), "", res);
  res <- gsub(paste0("^.*\\$", considerRoot, "\\$"), "\\$", res);
  return(res);
}
