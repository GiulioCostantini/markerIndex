markerIndex <- function(x, rotation = c("orthogonal", "oblique"), Phi,
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
      rotation <- "oblique"
    # extract structure loading matrix
    ld <- x$Structure
    class(ld) <- "matrix"
    pattern <- loadings(x)
    class(pattern) <- "matrix"

  } else
  {
    if(missing(rotation))
    {
      stop("A rotation needs to be specified")
    }
    if(rotation == "oblique" & missing(Phi))
    {
      stop("A Phi matrix of factor correlations needs to be specified")
    }

    # determine loadings
    if(rotation == "orthogonal")
    {
      ld <- x
      class(ld) <- "matrix"
    } else if(rotation == "oblique" & type.loading == "structure")
    {
      ld <- x
      class(ld) <- "matrix"
      pattern <- ld %*% solve(Phi)
    } else if(rotation == "oblique" & type.loading == "pattern")
  {
    ld <- as.matrix(x) %*% Phi
    pattern <- x
    class(pattern) <- "matrix"
  }
}

if(rotation == "orthogonal") Phi <- diag(ncol(ld))
ld_bkp <- ld
ld <- abs(ld)

# create empty marker index object
mi <- ld
mi[] <- NA

if(rotation == "orthogonal")
{
  # for(i in 1:ncol(ld))
  #   mi[,i] <- 1-sqrt((1-ld[,i])^2 + rowSums(ld[,-i]^2))

  # alternative formulation
  for(i in 1:ncol(ld))
  {
    fk <- ld
    fk[,i] <- 1
    fk[,-i] <- 0
    mi[,i] <- 1-sqrt(rowSums((fk - ld)^2))
  }

} else if(rotation == "oblique")
{
  # for(i in 1:ncol(ld))
  # {
  #
  #   fk <- ld
  #   fk[,i] <- 1
  #   fk[,-i] <- 0
  #   mi[,i] <- 1 - apply(((fk-ld) %*% Phi) * (fk-ld), 1, sum)
  # }

  #alternative formulation
  for(k in 1:ncol(ld))
    for(i in 1:nrow(ld))
    {
      ai <- ld[i,]
      fk <- ld[i,]
      fk[k] <- 1
      fk[-k] <- 0
      mi[i, k] <- 1- sqrt(t(fk-ai) %*% Phi %*% (fk-ai))
    }

}

if(rotation == "orthogonal")
  loading <- ld_bkp
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


