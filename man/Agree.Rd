\name{Agree}
\alias{Agree}
\title{Raw Simple And Extended Percentage Agreement}
\description{
Computes raw simple and extended percentage agreement among raters.
}
\usage{
Agree(x, tolerance = 0, na.rm = FALSE)
}
\arguments{
  \item{x}{a data.frame or a \eqn{k \times m}{k x m} matrix, k subjects (in rows) m raters (in columns).}
  \item{tolerance}{number of successive rating categories that should be regarded as rater agreement (see details).}
  \item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. If set to \code{TRUE} only the complete cases of the ratings will be used. Defaults to \code{FALSE}.
  %%     ~~Describe \code{na.rm} here~~
}

  
}
\details{
Using extended percentage agreement (\code{tolerance != 0}) is only possible for numerical values. If tolerance equals 1, for example, raters differing by one scale degree are interpreted as agreeing.
}
\value{
numeric value of coefficient of interrater reliability

The number of finally (potentially after omitting missing values) used subjects and raters are returned as attributes:
  \item{subjects}{the number of subjects examined.}
  \item{raters}{the number of raters.}
}
\author{Matthias Gamer <m.gamer@uke.uni-hamburg.de>, \cr some editorial amendments Andri Signorell <andri@signorell.net>}
\seealso{\code{\link[DescTools]{CohenKappa}}, \code{\link[DescTools]{KappaM}}}
\examples{
categ <- c("V", "N", "P")
lvls  <- factor(categ, levels=categ)
rtr1  <- rep(lvls, c(60, 30, 10))
rtr2  <- rep(rep(lvls, nlevels(lvls)), c(53,5,2, 11,14,5, 1,6,3))
rtr3  <- rep(rep(lvls, nlevels(lvls)), c(48,8,3, 15,10,7, 3,4,2))

Agree(cbind(rtr1, rtr2))       # Simple percentage Agreement
Agree(data.frame(rtr1, rtr2))  # can be a data.frame
Agree(cbind(rtr1, rtr2, rtr3)) # Simple percentage Agreement

Agree(cbind(rtr1, rtr2), 1)    # Extended percentage Agreement
}
\keyword{univar}
