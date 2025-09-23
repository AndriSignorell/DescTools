\name{Agree}
\alias{Agree}

\title{Raw Simple And Extended Percentage Agreement}
\description{
Computes raw simple and extended percentage agreement among raters.
}
\usage{
Agree(x, conf.level = 0.95, fpc = 0, verbose = 0)
}
\arguments{
  \item{x}{a \eqn{k \times m}{k x m} data.frame or matrix, k subjects (in rows) m raters (in columns). 
  }
  \item{conf.level}{the confidence level}
  \item{fpc}{ fpc }
  \item{verbose}{ for more results }
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

# in order to remove subjects with missing ratings casewise use:
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


# *********************************************************
# Operating the Formula Interface

# we have 5 subjects and 4 raters (in wide form)
(d.ratings <- data.frame(
  subj = c("1", "2", "3", "4", "5"), 
  rtr1 = structure(c(1,1,1,1,3), 
                  levels = c("V", "N", "P"), class = "factor"), 
  rtr2 = structure(c(1,2,1,1,3), 
                  levels = c("V", "N", "P"), class = "factor"), 
  rtr3 = structure(c(1,3,1,1,3), 
                  levels = c("V", "N", "P"), class = "factor"), 
  rtr4 = structure(c(1,1,1,1,2), 
                  levels = c("V", "N", "P"), class = "factor")
  )) 

# this structure can be directly send to the function
Agree(d.ratings[, -1])

# or we can coerce to a matrix before (if we want..)
Agree(as.matrix(d.ratings[, -1]))

# but what, when our structure is in long format?        
d.long <- reshape(d.ratings,
                  varying=2:5,
                  idvar=c("subj"),
                  times=colnames(d.ratings)[2:5],
                  v.names="rat", timevar="rater",
                  direction="long",
                  new.row.names=seq(prod(dim(d.ratings))))
head(d.long)

# for that we can use the formula interface of RaterFrame()
# which returns a wide form of subjects and raters.
# note the structure: response ~ subjects (rows) | raters (columns)
Agree(RaterFrame(rat ~ subj | rater, d.long))

# but what, when we have missings?
# we leave out rtr1's rating for subj 2 

# if we do nothing, we clearly get NA
Agree(RaterFrame(rat ~ subj | rater, d.long[-2,]))

# setting na.action to na.omit omits the subject "1" 
# as reported in attr(,"na.action"), number of subjects is yet 4
Agree(RaterFrame(rat ~ subj | rater, d.long[-2,], na.action = na.omit))

# setting na.rm only ignores missings, subject 1 remains in the matrix
Agree(RaterFrame(rat ~ subj | rater, d.long[-2,], na.rm = TRUE))
}



\keyword{univar}
\concept{ irr }
