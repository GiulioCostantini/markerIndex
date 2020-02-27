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
library(psych)
library(GPArotation)
library(psychTools)

#### Example 1 ####
# reproduce the example in Table 1 in Gallucci & Perugini (2007)

Table1 <- matrix(c(.70, .10, .06,
                   .70, .35, .35,
                   .42, .01, .01,
                   .40, .31, .22,
                   .60, .10, -.10,
                   .60, .50, .40,
                   .70, .70, .01,
                   .21, .70, .10),
                 nrow = 8, ncol = 3, byrow = TRUE)

MI <- markerIndex(Table1, rotation = "orthogonal")
MI

# a nicer way to print the marker indices
print(MI, digits = 2, cutoff = .2, sort = TRUE, what = "markerIndex")

# one can also print the factor solution, organized by the highest marker index by factor
print(MI, digits = 2, cutoff = .2, sort = TRUE, what = "full")

# if oblique rotation is specified, but factors are uncorrelated, the same
# results is obtained
MI <- markerIndex(Table1, rotation = "oblique", Phi = diag(3),
                  type.loading = "structure")
MI

#### Example 2 ####
# illustrative example with loadings in extreme situations of perfect or zero loading
Lambda <- matrix(
  c(
    # item 1 loads perfectly on the first factor (it should have MI 1 on factor
    # 1 and 1-sqrt(2) on all other factors)
    1, 0, 0,
    # items 2 and 3 are a similar case, but on the remaining two factors
    0, 1, 0,
    0, 0, 1,
    # item 4 does not load on any factor, it should have MI = 0 for all factors
    0, 0, 0),
  nrow = 4, ncol = 3, byrow = TRUE)

markerIndex(Lambda, rotation = "orthogonal")


#### Example 3 ####
# This is an example with real data and varimax rotation
library(psych)
x <- principal(Harman74.cor$cov, nfactors = 4, rotate="varimax")
MI <- markerIndex(x)
print(MI, what = "full", digits = 2, cutoff = .2, sort = TRUE)

# Same with oblique rotation
x <- principal(Harman74.cor$cov, nfactors = 4, rotate="oblimin")
MI <- markerIndex(x)
print(MI, what = "full", digits = 2, cutoff = .2, sort = TRUE)

# Same example, in which the pattern loading matrix and
# factor correlations are provided manually
MI <- markerIndex(loadings(x), rotation = "oblique", Phi = x$Phi,
                  type.loading = "pattern")
print(MI, what = "full", digits = 2, cutoff = .2, sort = TRUE)

# The same results are obtained if the structure matrix is provided instead
MI <- markerIndex(x$Structure, rotation = "oblique", Phi = x$Phi,
                  type.loading = "structure")
print(MI, what = "full", digits = 2, cutoff = .2, sort = TRUE)


}
\keyword{marker index}
\keyword{factor analysis}
\keyword{principal component analysis}
\keyword{rotation}
