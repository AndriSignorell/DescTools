\name{Agree}
\alias{Agree}
\title{Raw Simple And Extended Percentage Agreement}
\description{
Computes raw simple and extended percentage agreement among raters.
}
\usage{
Agree(x, grp = NULL, tolerance = 0, na.rm = FALSE)
}
\arguments{
  \item{x}{a data.frame, a list or a \eqn{k \times m}{k x m} matrix, k subjects (in rows) m raters (in columns). 
  If \code{grp} is provided it must be a value vector of same length.}
  \item{grp}{a grouping vector, if this is provided, x must be a suitable value vector.}
  \item{tolerance}{number of successive rating categories that should be regarded as rater agreement (see details).}
  \item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. If set to \code{TRUE} only the complete cases of the ratings will be used. Defaults to \code{FALSE}.
  %%     ~~Describe \code{na.rm} here~~
}

  
}
\details{
Extended percentage agreement can be calculated with a \code{tolerance > 0}. If tolerance equals 1 e.g., 
raters differing by one scale degree are interpreted as agreeing. This makes sense with numeric ratings.
When used with characters it must be noted that those are coerced to factors and subsequently to numeric
values.
}
\value{
numeric value of coefficient of interrater reliability

The number of finally (potentially after omitting missing values) used subjects and raters are returned as attributes:
  \item{subjects}{the number of subjects examined.}
  \item{raters}{the number of raters.}
}
\author{Andri Signorell <andri@signorell.net>\cr (originally based on an idea of Matthias Gamer <m.gamer@uke.uni-hamburg.de>)}
\seealso{\code{\link[DescTools]{CohenKappa}}, \code{\link[DescTools]{KappaM}}}


\examples{

rating <- data.frame(
   subj   = LETTERS[seq(8)],
   rater1 = c(4, 2, 4, 4, 4, 4, 1, 4), 
   rater2 = c(4, 2, 4, 4, 3, 4, 3, 4), 
   rater3 = c(3, 2, 5, 4, 2, 3, 2, 3), 
   rater4 = c(4, 2, 5, 4, 4, 1, 5, 4))

# provide a data.frame
Agree(rating[, -1])

# provide a list
Agree(as.list(rating[, -1]))

# provide a matrix
Agree(as.matrix(rating[, -1]))

# provide value and group
with(ToLong(rating[, -1]), Agree(x, grp))

# provide a character matrix
chrm <- as.matrix(rating[, -1]) 
chrm[] <- as.character(factor(chrm, labels=letters[1:5]))
chrm
Agree(chrm)

# NA handling
rating[1, 1] <- NA
Agree(rating)

# ignores NAs - but does not remove the whole subject (!)
Agree(rating, na.rm=TRUE)

# in order to remove subjects with missing ratings listwise use:
Agree(na.omit(rating))

# Extended percentage agreement, allowing differences of max. 1
exam <- data.frame(
  subj   = LETTERS[seq(8)],
  rater1 = c(40, 21, 43, 40, 46, 46, 16, 49), 
  rater2 = c(40, 20, 43, 41, 34, 46, 36, 46), 
  rater3 = c(31, 20, 51, 42, 23, 35, 24, 37), 
  rater4 = c(42, 21, 55, 40, 46, 12, 56, 48))

# strictly we have no agreement
Agree(exam[, -1])

# release difference to max 1, we have 1 agreement (2)
Agree(exam[, -1], tolerance=1)

# release difference to max 2, we have 2 agreements (2, 4)
Agree(exam[, -1], tolerance=2)
}
\keyword{univar}
