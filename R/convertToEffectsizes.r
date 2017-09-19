convertToEffectsizes <- function(object, n, silent = TRUE) {

  if ('chisq' %IN% names(object) &&
        !is.na(object$chisq) &&
        'df1' %IN% names(object) &&
        !is.na(object$df1)) {
    if ('v' %IN% names(object)) {
      newName <- 'v.from.chisq';
    } else {
      newName <- 'v';
    }
    if (!is.na(n)) {
      object[[newName]] <- 
        convert.chisq.to.V(object$chisq,
                           n=n,
                           minDim=object$df1);
    } else {
      if (!silent) {
        cat("     No sample size available to compute V from Chi Square!\n");
      }
    }
  }
  
  if ('t' %IN% names(object) &&
        !is.na(object$t)) {
    if ('d' %IN% names(object)) {
      newName <- 'd.from.t';
    } else {
      newName <- 'd';
    }
    ### Check for degrees of freedom
    if (('df1' %IN% names(object)) &&
          (!is.na(object$df1))) {
      df <- df1;
    } else if (!is.na(n)) {
      df <- n - 2;
    }
    ### Convert t and store d
    if (!is.na(df)) {
      object[[newName]] <- 
        convert.t.to.d(t=object$t, df=df);
    } else if (!silent) {
      cat("     No sample size available to compute Cohen's d from t!\n");
    }
  }
  
  return(object);
  
}