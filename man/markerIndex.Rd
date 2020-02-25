\name{markerIndex}
\alias{markerIndex}
\title{
Computes the marker index proposed by Perugini and Gallucci (2007)
}
\description{
The marker index is the complement to 1 of the Euclidean distance between a variable and the vertex of the factor. In case of an oblique rotation, the Generalized Euclidean Distance is used, which accounts also for factor correlations.
}
\usage{
markerIndex(x, rotation = c("orthogonal", "oblique"), Phi,
    type.loading = c("structure", "pattern"))
}
\arguments{
  \item{x}{either a loading matrix or a \code{\link{principal}}/\code{\link{fa}} object from package \code{psych}. In the latter case, it is not necessary to specify other arguments, such as \code{rotation}, \code{Phi}, or \code{type.loading}
  }
  \item{rotation}{The type of rotation employed}
  \item{Phi}{In case of oblique rotation, a matrix Phi of correlations among factors needs to be specified}
  \item{type.loading}{In case of oblique rotation, the type of loading matrix provided needs to be specified, defaults to structure matrix}
}

\details{
If x is a psych object, no other arguments need to be specified.
}
\value{
An object of class markerindex, which is a list including the following objects
  \item{MI}{the marker indices of each item on each factor
  }
  \item{loadings}{The factor/component loadings on which the marker index was computed}
  \item{rotation}{the type of rotation}
  \item{Phi}{the matrix of correlations among components/factors}
  \item{type.loading}{In case of oblique rotation, whether loadings refer to the structure or the pattern matrix}
}
\references{
Perugini, M., Gallucci, M., & Livi, S. (2000). Looking for a Simple Big Five Factorial Structure in the Domain of Adjectives. European Journal of Psychological Assessment, 16(2), 87-97. https://doi.org/10.1027//1015-5759.16.2.87

Gallucci, M., & Perugini, M. (2007). The marker index: A new method of selection of marker variables in factor analysis. TPM - Testing, Psychometrics, Methodology in Applied Psychology, 14(1), 3-25.
}
\author{
Giulio Costantini
}
\seealso{
\code{\link{principal}}
\code{\link{fa}}
}
\examples{
c
}
\keyword{marker index}
\keyword{factor analysis}
\keyword{principal component analysis}
\keyword{rotation}
