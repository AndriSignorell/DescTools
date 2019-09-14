
% File src/library/base/man/Quot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Quot}
\alias{Quot}

\title{Lagged Quotients}
\description{
  Returns suitably lagged and iterated quotients
}
\usage{
  Quot(x, lag = 1, quotients = 1, \dots)

}
\arguments{
  \item{x}{a numeric vector or matrix containing the values to be
    used for calculating the quotients.}
  \item{lag}{an integer indicating which lag to use.}
  \item{quotients}{an integer indicating the order of the quotient.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{\link{NA}}'s propagate.
}
\value{
  If \code{x} is a vector of length \code{n} and \code{quotients = 1},
  then the computed result is equal to the successive quotients
  \code{x[(1+lag):n] - x[1:(n-lag)]}.

  If \code{quotients} is larger than one this algorithm is applied
  recursively to \code{x}.
  Note that the returned value is a vector which is shorter than
  \code{x}.

  If \code{x} is a matrix then the division operations are carried out
  on each column separately.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{diff}}
}
\examples{
Quot(1:10, 2)
Quot(1:10, 2, 2)
x <- cumprod(cumprod(1:10))
Quot(x, lag = 2)
Quot(x, quotients = 2)
}
\keyword{arith}