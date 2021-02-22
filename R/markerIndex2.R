# currently undocumented
# calculate oblique factor marker index using the dist function
# calculate oblique factor marker index by projecting the oblique factors into
# an orthogonal space and than using the dist function

markerIndex2 <- function(x, data, rotation = c("orthogonal", "oblique"), Phi,
                          type.loading = c("structure", "pattern"))
{
  rotation <- match.arg(rotation)
  type.loading <- match.arg(type.loading)
  
  if(any(c("fa", "principal") %in% class(x)))
  {
    # determine rotation (orthogonal or oblique), loading matrix, and factor correlation matrix Phi
    Phi <- x$Phi
    if(is.null(Phi))
    {
      rotation <- "orthogonal"
      # extract loading matrix
      ld <- loadings(x)
      class(ld) <- "matrix"
      
    } else
    {
      rotation <- "oblique"
      # extract structure loading matrix
      ld <- x$Structure
      class(ld) <- "matrix"
      pattern <- loadings(x)
      class(pattern) <- "matrix"
    }
  } else # extract info in case the input is not a pca/fa psych object
  {
    if(missing(rotation))
    {
      stop("A rotation needs to be specified")
    }
    if(rotation == "oblique" & missing(Phi))
    {
      stop("A Phi matrix of factor correlations needs to be specified")
    }
    if(rotation == "oblique" & missing(data))
    {
      stop("A data matrix needs to be included in case of oblique rotation")
    }
    
    # determine loadings
    if(rotation == "orthogonal")
    {
      ld <- x
      class(ld) <- "matrix"
      
    } else if(rotation == "oblique")
    {
      if(type.loading == "structure")
      {
        ld <- x
        class(ld) <- "matrix"
        pattern <- ld %*% solve(Phi)
        
      } else if (type.loading == "pattern")
      {
        ld <- as.matrix(x) %*% Phi
        pattern <- x
        class(pattern) <- "matrix"
      }
      nf <- ncol(ld)
    }
  }
  
  # calculate markerIndex - orthogonal rotation
  if(rotation == "orthogonal") 
  {
    nf <- ncol(ld)
    Phi <- diag(nf)
    # calculate euclidean distance between factors and items
    ds <- dist(rbind(Phi, abs(ld)), method = "euclidean")
    ds <- as.matrix(ds)
    ds <- ds[-c(1:nf), 1:nf]
    # marker index is just the complement to one of this distance
    mi <- 1-ds
  }  else if (rotation == "oblique") 
  {
    # calculate markerIndex - oblique rotation
    nf <- ncol(ld)    
    
    # calculate the corresponding unrotated solution, in which we can embed 
    # oblique factors
    ort <- principal(data, nfactor = nf)
    
    # project oblique factors into the same orthogonal space
    fc <- fa.congruence(pattern, ort$loadings, structure = F, digits = 10)
    
    # compute the distance between oblique factors and items in the orthogonal space
    ds1 <- dist(rbind(fc, ort$loadings), method = "euclidean")
    ds1 <- as.matrix(ds1)
    ds1 <- ds1[-c(1:nf),1:nf]

    # compute the distance between the other pole of oblique factors 
    # and items in the orthogonal space,
    ds2 <- dist(rbind(-1*fc, ort$loadings), method = "euclidean")
    ds2 <- as.matrix(ds2)
    ds2 <- ds2[-c(1:nf),1:nf]
    
    # keep the minimum of the two distances
    dsA <- array(dim = c(dim(ds1), 2))
    dsA[,,1] <- ds1
    dsA[,,2] <- ds2
    
    ds <- apply(dsA, 1:2, min)    
    # marker index is just the complement to one of this distance
    mi <- 1-ds
  }    
  
  if(rotation == "orthogonal")
    loading <- ld
  else if (rotation == "oblique")
    loading <- pattern
  
  MI <- list(MI = mi,
             loadings = loading,
             rotation = rotation,
             Phi = Phi,
             type.loading = ifelse(rotation == "orthogonal", "NA", type.loading))
  class(MI) <- "markerindex"
  MI
}


