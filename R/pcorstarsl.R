# adapted from here to include partial correlation matrix
# https://github.com/kyuni22/ksmv/blob/master/functions/corstarsl.R

pcorstarsl <- function(pcm, n, np, digits = 2, full = FALSE){ 
  # pcm = partial correlation matrix
  # n = sample size
  # np = number of variables partialled out

  if(is.null(rownames(pcm)))
    rownames(pcm) <- 1:nrow(pcm)
  if(is.null(colnames(pcm)))
    colnames(pcm) <- 1:ncol(pcm)
  
  R <- pcm
  p <- corr.p(R, n-np, adjust = "none")$p
  
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "** ", 
                           ifelse(p < .05, "* ", ifelse(p < .1, "+ ", " "))))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(R, digits))
  R <- apply(R, 2, str_replace, "0.", ".")
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(pcm)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(pcm) 
  colnames(Rnew) <- paste(colnames(pcm), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  if(full)
    out <- list("Rformatted" = Rnew, "R" = pcm, "p" = p)
  else
    out <- Rnew
  
  out
}

