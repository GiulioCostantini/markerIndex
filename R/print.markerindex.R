print <- function(x, ...){
  UseMethod("print")
}

print.markerindex <- function(x, digits = NULL, cutoff = NULL, sort = FALSE, what = c("markerIndex", "full"), ...)
{
  what <- match.arg(what)
  ld <- x$loadings
  MI <- x$MI
  rotation <- x$rotation
  Phi <- x$Phi
  type.loading <- x$type.loading
  nfactor <- ncol(ld)
  
  
  if(is.null(colnames(ld))) colnames(ld) <- 1:ncol(ld)
  if(is.null(rownames(ld))) rownames(ld) <- 1:nrow(ld)
  itms <- rownames(ld)
  
  # identify highest marker index for each item and the corresponding factor
  MaxMi <- apply(MI, 1, max)
  fct <- colnames(ld)[apply(MI, 1, which.max)]
  
  
  if(what == "markerIndex")
  {
    mi <- data.frame(itms, MI, "factor" = fct, MaxMi)
    if(!missing(cutoff))
      mi[,seq(2, nfactor+1)][abs(MI) < cutoff] <- NA
    
    if(!missing(digits))
      mi[,seq(2, nfactor+1)] <- round(mi[,seq(2, nfactor+1)], digits)

    if(sort) 
      mi <- arrange(mi, match(mi$factor, colnames(ld)), -MaxMi)

    rownames(mi) <- mi$itms
    mi <- dplyr::select(mi, -itms, -MaxMi, -factor)
    out <- mi
    
  } else if (what == "full") {
    
    LD <- data.frame(itms, ld, "factor" = fct, MaxMi)
    
    if(!missing(cutoff))
      LD[,seq(2, nfactor+1)][abs(LD[,seq(2, nfactor+1)]) < cutoff] <- NA
    
    if(!missing(digits))
      LD[,seq(2, nfactor+1)] <- round(LD[,seq(2, nfactor+1)], digits)
    
    if(sort & !is.null(colnames(ld))) 
        LD <- arrange(LD, match(LD$factor, colnames(ld)), -MaxMi)
    
    rownames(LD) <- LD$itms
    
    if(!missing(digits))
      LD[,"MaxMi"] <- round(LD[,"MaxMi"], digits)
    
    LD <- rename(LD, MI = MaxMi)
    LD <- select(LD, -itms)
    out <- LD
    
  }
  
  out[is.na(out)] <- ""
  
  print(out)
  invisible(out)
}