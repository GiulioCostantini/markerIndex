\name{scaleScores}
\alias{scaleScores}
\title{
Compute scale scores
}
\description{
Compute scale scores exploiting regularities in the item names.
}
\usage{
scaleScores(data, pattern, recode = FALSE, recString = "_r", min = 1,
max = 5, na.rm = TRUE)
}
\arguments{

  \item{data}{
A dataframe.
}
  \item{pattern}{
A list of named items. Each element of the list identifies a set of items composing a scale, through a vector of strings. Each scale is identified by a vector of strings. All items whose names include all the strings specified in the vector are included in the scale.
}
 \item{recode}{
Logical. Whether reverse-coded items should be recoded before estimating reliability.
}
 \item{recString}{
A string that identifies reverse-coded items, by default "_r". Be careful: if \code{recode = TRUE}, all items including this string in their names will be recoded.
}
  \item{min}{
The lowest point of the response scale
}
  \item{max}{
The highest point of the response scale
}
  \item{na.rm}{
Whether NAs should be removed before computing scale score
}
}
\value{
A dataframe of scale scores
}
\author{
Giulio Costantini
}
\examples{
library(psychTools)
data(bfi)

# bfi.keys include information on reverse-scoring. In particular, items with the string
# "-" are reverse-scored. We can exploit this information to reverse-score all items
bfi2 <- bfi
names(bfi2)[1:25] <- unlist(bfi.keys)

# scale scores can be computed with function scaleScores
BFI <- scaleScores(bfi2, pattern = list(
  "Openness" = c("O"),
  "Conscientiousness" = c("C"),
  "Extraversion" = c("E"),
  "Agreeableness" = c("A"),
  "Neuroticism" = c("N")),
  recode = TRUE,
  recString = "-",
  min = 1, max = 6)
head(BFI)
}
