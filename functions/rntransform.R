rntransform = function(x){
  ## ztransform
  mids <- (!is.na(x))
  resid <- x[mids]    
  x <- (resid - mean(resid))/sd(resid)

  ## RNT
  o <- rank(x, ties.method = "random") - 0.5
  o[is.na(x)] <- NA
  mP <- 0.5/max(o, na.rm = T)
  o <- o/(max(o, na.rm = T) + 0.5)
  o <- qnorm(o)
  
  tmeas <- as.logical(mids)
  out <- rep(NA, length(mids))
  out[tmeas] <- o
  out
}
