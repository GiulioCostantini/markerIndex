\name{pcorstarsl}
\alias{pcorstarsl}
\title{
Partial correlation matrix with singificance stars
}
\description{
Format a partial correlation matrix with stars to indicate significance
}
\usage{
pcorstarsl(pcm, n, np, digits = 2, full = FALSE)
}
\arguments{
  \item{pcm}{
A partial correlation matrix
}
  \item{n}{
Sample size on which the partial correlation matrix \code{pcm} was computed.
}
  \item{np}{
Number of variables partialled out
}
  \item{digits}{
number of decimal places to use
}
  \item{full}{
logical. If TRUE, a more complete output is returned, including not only the formatted partial correlation matrix, but also the original partial correlations and p-values.
}
}
\details{
P-values for partial correlations are computed using function \code{\link[psych]{corr.test}}
}
\value{
if \code{full} is false, a formatted partial correlation matrix is returned. Otherwise, a list including the formatted partial correlation matrix, the original partial correlaitons and the p-values.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Hmisc]{rcorr}}
\code{\link[psych]{corr.test}}
\code{\link{corstarsl}}
}

\examples{
library(psychTools)
library(corpcor)
data(bfi)

# compute correlation matrix using listwise deletion
cm <- cor(bfi[,1:10], use = "complete.obs")

# convert it to partial correlation matrix, using funciton cor2pcor in package corpcor
pcm <- cor2pcor(cm)

# include variable names in the partial correlation matrix
colnames(pcm) <- rownames(pcm) <- colnames(cm)

# determine sample size as the number of cases without missing values
n <- sum(rowSums(is.na(bfi)) == 0)

# format the partial correlation matrix.
# parameter np is set to eight, because from each correlation between two variables
# all remaining eight variables were partialled out
pcorstarsl(pcm, n, np = 8)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ partial correlation }% use one of  RShowDoc("KEYWORDS")
\keyword{ p-value }% __ONLY ONE__ keyword per line
