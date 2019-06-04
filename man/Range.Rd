\name{Range}
\alias{Range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{(Robust) Range
%%  ~~function to do ... ~~
}
\description{
Determines the range of the data, which can possibly be trimmed before calculating the extreme values. The robust range version is calculated on the basis of the trimmed mean and variance (see Details).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Range(x, trim = NULL, robust = FALSE, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.
    Values of trim outside that range are taken as the nearest endpoint. Default is 0 for \code{robust=FALSE} and 0.2 for \code{robust=TRUE}
  }
  \item{robust}{logical, determining whether the robust or the convential range should be returned.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
  \item{...}{the dots are sent to \code{RobRange} and can be used to set \code{fac} (See details).}

}
\details{The R base function range returns the minimum and maximum value of a numeric object. Here we return the span of a (possibly trimmed) numeric vector, say the difference of maximum and minimum value.

If robust is set to \code{TRUE} the function determines the trimmed mean m and then the "upper trimmed mean" s of absolute deviations
from m, multiplied by \code{fac} (fac is 3 by default). The robust minimum is then defined as m-fac*s or min(x),
whichever is larger, and similarly for the maximum.

}
\value{
  If \code{trim} is zero (the default), the arithmetic mean of the
  values in \code{x} is computed, as a numeric or complex vector of
  length one.  If \code{x} is not logical (coerced to numeric), numeric
  (including integer) or complex, \code{NA_real_} is returned, with a warning.

  If \code{trim} is non-zero, a symmetrically trimmed mean is computed
  with a fraction of \code{trim} observations deleted from each end
  before the mean is computed.
}
\author{Werner Stahel, ETH Zurich (robust range)\cr
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{range}}, \code{\link{min}}, \code{\link{max}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(0:10, 50)
xm <- Range(x)
c(xm, Range(x, trim = 0.10))

x <- c(rnorm(20), rnorm(3, 5, 20))
Range(x, robust=TRUE)

# compared to
Range(x)
}
\keyword{ univar}
