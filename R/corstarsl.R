# adapted from here
# https://github.com/kyuni22/ksmv/blob/master/functions/corstarsl.R
corstarsl <- function(x, type = c("pearson", "spearman"), digits = 2, full = FALSE)
{ 
  type <- match.arg(type)
  x <- as.matrix(x) 
  Rbkp <- R <- rcorr(x, type = type)$r 
  p <- rcorr(x, type = type)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "** ", 
                           ifelse(p < .05, "* ", ifelse(p < .1, "+ ", " "))))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(R, digits))
  R <- apply(R, 2, str_replace, "0.", ".")
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  if(full)
    out <- list("Rformatted" = Rnew, "R" = Rbkp, "p" = p)
  else
    out <- Rnew
  
  return(out)
}

