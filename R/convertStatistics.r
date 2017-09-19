convertStatistics <- function(dat) {
  ### Process each row separately; first add unique row numbers
  res <- dat;
  res$uniqueRowNumber <- 1:nrow(res);
  res <- ddply(res, 'uniqueRowNumber', function(x) {
    ### Convert all names to lowercase temporarily
    originalNames <- names(x);
    names(x) <- tolower(names(x));
    
    ### For Chi square
    if (is.nr(x$chisq) && is.nr(x$df1)) {
      ### Compute p-value
      x$p.computed.from.chisq <- convert.chisq.to.p(x$chisq, x$df1);
      ### If we can, compute Cramer's V
      if (is.nr(x$size) && is.nr(x$df1) && is.nr(x$size)) {
        x$v.computed.from.chisq <- convert.chisq.to.V(chisq = x$chisq,
                                                      n = x$size,
                                                      minDim = x$df1);
      }
    }
    
    ### For F-values
    if (is.nr(x$f) && is.nr(x$df1) && is.nr(x$df2)) {
      ### Compute p-value
      x$p.computed.from.f <- convert.f.to.p(x$f, x$df1, x$df2);
      ### If we can, compute Cohen's d
      if (x$df1 == 1) {
        x$d.computed.from.f <- convert.f.to.d(x$f, x$df1, x$df2);
        x$etasq.computed.from.f <- convert.f.to.etasq(x$f, x$df1, x$df2);
        x$omegasq.computed.from.f <- convert.f.to.omegasq(x$f, x$df1, x$df2);
      } else {
        x$etasq.computed.from.f <- convert.f.to.etasq(x$f, x$df1, x$df2);
        x$omegasq.computed.from.f <- convert.f.to.omegasq(x$f, x$df1, x$df2);
      }
    }
    
    ### For t-values
    if (is.nr(x$t) && (is.nr(x$df1) || is.nr(x$size))) {
      if (is.nr(x$df1)) {
        df <- x$df1;
      } else {
        df <- x$size;
      }
      ### Compute Pearson's r
      x$r.computed.from.t <- convert.t.to.r(x$t, x$size) 
      ### Compute p-value
      x$p.computed.from.t <- convert.t.to.p(x$t, df);
      ### Compute Cohen's d
      x$d.computed.from.t <- convert.t.to.d(x$t, df);
    }
    
    ### For OR values
    if (is.nr(x$or)) {
      ### Compute Pearson's r
      x$r.computed.from.or <- convert.or.to.r(x$or)
      ### Compute Cohen's d
      x$d.computed.from.or <- convert.or.to.d(x$or);
    }
    
    ### Convert the old names back to their original cases
    names(x)[1:length(originalNames)] <- originalNames;
    ### Return result
    return(x);
  });
  ### Remove row identifier
  res$uniqueRowNumber <- NULL;
  return(res);
}
