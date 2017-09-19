obtainP <- function(object, warn=FALSE) {
  available <- tolower(names(object)[!is.na(object)]);
  
  ### Convert degrees of freedom to n (unless the df is for chi square)
  if (is.na(object$n) && !is.na(object$df1) && !('chisq' %in% available)) {
    object$n <- object$df1 + 1;
  }
  if (('t' %in% available) && ('df1' %in% available)) {
    object$p.computed <- convert.t.to.p(object$t, object$df1);
  } else if (('t' %in% available) && ('n' %in% available)) {
    object$p.computed <- convert.t.to.p(object$t, object$n - 1);
  } else if (('b' %in% available) && ('se' %in% available) && ('n' %in% available)) {
    t <- convert.b.to.t(object$b, object$se);
    object$p.computed <- convert.t.to.p(t, object$n - 1);
    
  } else if (('r' %in% available) && ('n' %in% available)) {
    t <- convert.r.to.t(object$r, object$n);
    object$p.computed <- convert.t.to.p(t, object$n - 1);
  } else if (('f' %in% available) && ('df1' %in% available) && ('df2' %in% available)) {
    ### Compute p-value
    object$p.computed <- convert.f.to.p(object$F, object$df1, object$df2);
  } else {
    if (warn) {
      warning(paste0("Unable to convert a p-value for ", deparse(substitute(object)),
                     ". Available data: ", paste0(available, collapse=", ")));
    }
    object$p.computed <- NA;
  }
  return(object)
}