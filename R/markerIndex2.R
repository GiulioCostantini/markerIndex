# currently undocumented
# calculate oblique factor marker index using the dist function
# calculate oblique factor marker index by projecting the oblique factors into
# an orthogonal space and than using the dist function

# works only on a "principal" object from psych

markerIndex2 <- function(x, data)
{

  # extract loadings
  ld <- x$Structure
  class(ld) <- "matrix"
  pattern <- loadings(x)
  
  # calculate markerIndex - oblique rotation
  nf <- ncol(ld)    
  
  # calculate the corresponding unrotated solution, in which we can embed 
  # oblique factors
  ort <- principal(data, nfactors = nf)
  
  # project oblique factors into the same orthogonal space
  fc <- cor(x$scores, ort$scores)
  
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

  MI <- list(MI = mi,
             loadings = pattern,
             rotation = "oblique",
             Phi = x$Phi,
             type.loading = "pattern")
  class(MI) <- "markerindex"
  MI
}


