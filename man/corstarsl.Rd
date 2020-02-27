\name{corstarsl}
\alias{corstarsl}
\title{
Correlation matrix with singificance stars
}
\description{
Format a correlation matrix with stars to indicate significance
}
\usage{
corstarsl(x, type = c("pearson", "spearman"), digits = 2, full = FALSE)
}

\arguments{
  \item{x}{
data matrix
}
  \item{type}{
type of correlation matrix, to be passed to \code{\link[Hmisc]{rcorr}}.
}
  \item{digits}{
number of decimal places to use
}
  \item{full}{
logical. If TRUE, a more complete output is returned, including not only the formatted correlation matrix, but also the original correlations and p-values.
}
}
\details{
Missing values are treated using pairwise deletion, which is the default of \code{\link[Hmisc]{rcorr}}
}
\value{
if \code{full} is false, a formatted correlation matrix is returned. Otherwise, a list including the formatted correlation matrix, the original correlaitons and the p-values.

}
\references{
https://github.com/kyuni22/ksmv/blob/master/functions/corstarsl.R
}
\author{
Giulio Costantini
}
\note{
This funciton was adapted from here: https://github.com/kyuni22/ksmv/blob/master/functions/corstarsl.R
}

\seealso{
\code{\link[Hmisc]{rcorr}}
\code{\link{pcorstarsl}}
}
\examples{
library(psychTools)
data(bfi)
corstarsl(bfi[,1:10])
}
\keyword{ correlation }
\keyword{ p-values }
