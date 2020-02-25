\name{print.markerindex}
\alias{print.markerindex}
\title{
Print a \code{markerindex} object.
}
\description{
Can be used either to print the marker index table, or to print factor/component loadings together with the highest marker index for each factor/component. 
}
\usage{
\method{print}{markerindex}(x, digits = NULL, cutoff = NULL, sort = FALSE, what = c("markerIndex", "full"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A markerindex object, typically the output of function markerIndex}
  \item{cutoff}{
Values smaller than this (in absolute value) are suppressed.
}
  \item{digits}{
Number of decimal places to use in printing marker indices and loadings.
}
  \item{sort}{
logical. If true, the variables are sorted by their highest marker index.
}
  \item{what}{
If \code{"markerIndex"}, only print the marker index table.
If \code{"full"}, print the loading matrix and the marker index.
}
\item{...}{
More options to pass to print. Currently not used.
}
}
\details{
This function is particularyl useful to print loadings in order of their highest marker index. This function is inspired by \code{\link{print.loadings}}
}
\value{
Returns a matrix including either the marker index values or the loadings and the highest marker index, organized as they are printed.
}
\references{
Perugini, M., Gallucci, M., & Livi, S. (2000). Looking for a Simple Big Five Factorial Structure in the Domain of Adjectives. European Journal of Psychological Assessment, 16(2), 87-97. https://doi.org/10.1027//1015-5759.16.2.87

Gallucci, M., & Perugini, M. (2007). The marker index: A new method of selection of marker variables in factor analysis. TPM - Testing, Psychometrics, Methodology in Applied Psychology, 14(1), 3-25.
}
\author{
Giulio Costantini
}
\seealso{
\code{\link{markerIndex}}
}
