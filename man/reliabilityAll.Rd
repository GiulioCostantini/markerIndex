\name{reliabilityAll}
\alias{reliabilityAll}
\title{
Compute Cronbach's Alpha and McDonald's Omega for a set of scales at once.
}
\description{
One often needs to estimate reliability for a large set of scales. This function does that in a convenient way, exploiting regularities in the naming of the items belonging to each scale.
}
\usage{
reliabilityAll(data, pattern, simplify = TRUE, recode = FALSE,
recString = "_r", omega = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataframe.
}
  \item{pattern}{
A list of named items. Each element of the list identifies a set of items composing a scale, through a vector of strings. Each scale for which reliability is computed is identified by a vector of strings. All items whose names include all the strings specified in the vector are included in the scale.
}

 \item{simplify}{
Logical. If true, format the results in a table, otherwise return the raw results from functions \code{\link[psych]{alpha}} and  \code{\link[MBESS]{ci.reliability}}.
}

 \item{recode}{
Logical. Whether reverse-coded items should be recoded (i.e., multiplied by -1) before estimating reliability. See argument \code{rec}.
}
 \item{recString}{
A string that identifies reverse-coded items, by default "_r". Be careful: if \code{recode = TRUE}, all items including this string in their names will be recoded before estimatting reliability.
}
\item{omega}{
Logical. Whether coefficient omega should be estimated.
}
}

\details{
Cronbach's alpha is estimated through \code{\link[psych]{alpha}} using default settings. McDonald's Omega is estimated through \code{\link[MBESS]{ci.reliability}} using argument \code{type == NULL}, which results in estimating hierarchical omega for continuous items and categorical omega for categorical items. 

}
\value{
if \code{simplify = TRUE}, a data.frame including the following items for each scale

  \item{scale}{the name of the scale}
  \item{nitem}{the number of items in the scale}
  \item{raw_alpha}{the raw alpha value from \code{\link[psych]{alpha}}}
  \item{std_alpha}{the std alpha value from \code{\link[psych]{alpha}}}
  \item{omega}{the omega estimate from \code{\link[MBESS]{ci.reliability}}}
  \item{errorsAlpha}{error messages returned by function \code{\link[psych]{alpha}}}
  \item{warningsAlpha}{warnings returned by function \code{\link[psych]{alpha}}}
  \item{errorsOmega}{error messages returned by function \code{\link[MBESS]{ci.reliability}}}
  \item{warningsOmega}{warnings returned by function \code{\link[MBESS]{ci.reliability}}}
  
Otherwise, a list including the output of  \code{\link[psych]{alpha}} and  \code{\link[MBESS]{ci.reliability}} for each scale.
}
\references{

Cronbach, L.J. (1951) Coefficient alpha and the internal strucuture of tests. Psychometrika, 16, 297-334.

Kelley, K. & Pornprasertmanit, P. (2016). Confidence intervals for population reliability coefficients: Evaluation of methods, recommendations, and software for homogeneous composite measures. Psychological Methods 21(1), 69-92.  https://doi.org/10.1037/a0040086

McDonald, R. P. (1999). Test theory: A unified approach. Mahwah, New Jersey: Lawrence Erlbaum Associates, Publishers.

Revelle, W. (in preparation) An introduction to psychometric theory with applications in R.
Springer. (Available online at https://personality-project.org/r/book).

}
\author{
Giulio Costantini
}
\seealso{
\code{\link[psych]{alpha}}, \code{\link[MBESS]{ci.reliability}}
}
\examples{
library(psychTools)
data(bfi)

# compute alpha and omega for dataset bfi in psychTools. Warnings and negative values
# of alpha reflect the fact that items were not reverse-scored
reliabilityAll(bfi,
               pattern = list(
                 "Openness" = c("O"),
                 "Conscientiousness" = c("C"),
                 "Extraversion" = c("E"),
                 "Agreeableness" = c("A"),
                 "Neuroticism" = c("N")))



# bfi.keys include information on reverse-scoring. In particular, items with the string
# "-" are reverse-scored. We can exploit this information to reverse-score all items
bfi2 <- bfi
names(bfi2)[1:25] <- unlist(bfi.keys)

reliabilityAll(bfi2, pattern = list(
  "Openness" = c("O"),
  "Conscientiousness" = c("C"),
  "Extraversion" = c("E"),
  "Agreeableness" = c("A"),
  "Neuroticism" = c("N")),
  recode = TRUE,
  recString = "-")

}
\keyword{alpha}
\keyword{omega}
\keyword{reliability}
