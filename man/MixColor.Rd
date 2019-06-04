\name{MixColor}
\alias{MixColor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the Convex Combination of Two Colors}
\description{
  This function can be used to compute the result of color
  mixing (it assumes additive mixing).
}
\usage{
MixColor(col1, col2, amount1 = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col1}{the first color.}
  \item{col2}{the second color.}
  \item{amount1}{the amount of color1. The amount of color2 results in (1-amount1).}
}
\value{
  The mixed color as hexstring
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{
   \code{\link{colorRamp}}, \code{\link{rgb}}
}

\examples{
# a mix between red and yellow with rates 3:7
MixColor("red", "yellow", 0.3)
}
\keyword{color}
