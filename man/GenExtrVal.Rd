\name{GenExtrVal}
\alias{dGenExtrVal}
\alias{pGenExtrVal}
\alias{qGenExtrVal}
\alias{rGenExtrVal}
\title{The Generalized Extreme Value Distribution}
\description{
  Density function, distribution function, quantile function and
  random generation for the generalized Extreme value (GenExtrVal)
  distribution with location, scale and shape parameters.
}
\usage{
dGenExtrVal(x, loc=0, scale=1, shape=0, log = FALSE)
pGenExtrVal(q, loc=0, scale=1, shape=0, lower.tail = TRUE)
qGenExtrVal(p, loc=0, scale=1, shape=0, lower.tail = TRUE)
rGenExtrVal(n, loc=0, scale=1, shape=0)
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{loc, scale, shape}{Location, scale and shape parameters; the
    \code{shape} argument cannot be a vector (must have length one).}
  \item{log}{Logical; if \code{TRUE}, the log density is returned.}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities
    are P[X <= x], otherwise, P[X > x]}
}
\details{
  The GenExtrVal distribution function with parameters
  \eqn{loc = a}, \eqn{scale = b} and
  \eqn{shape = s} is
  \deqn{G(z) = \exp\left[-\{1+s(z-a)/b\}^{-1/s}\right]}{
    G(x) = exp[-{1+s(z-a)/b}^(-1/s)]}
  for \eqn{1+s(z-a)/b > 0}, where \eqn{b > 0}.
  If \eqn{s = 0} the distribution is defined by continuity.
  If \eqn{1+s(z-a)/b \leq 0}{1+s(z-a)/b <= 0}, the value \eqn{z} is
  either greater than the upper end point (if \eqn{s < 0}), or less
  than the lower end point (if \eqn{s > 0}).

  The parametric form of the GenExtrVal encompasses that of the Gumbel,
  Frechet and reverse Weibull distributions, which are obtained
  for \eqn{s = 0}, \eqn{s > 0} and \eqn{s < 0} respectively.
  It was first introduced by Jenkinson (1955).
}
\value{
  \code{dGenExtrVal} gives the density function, \code{pGenExtrVal} gives the
  distribution function, \code{qGenExtrVal} gives the quantile function,
  and \code{rGenExtrVal} generates random deviates.
}
\references{
  Jenkinson, A. F. (1955)
  The frequency distribution of the annual maximum (or minimum) of
  meteorological elements.
  \emph{Quart. J. R. Met. Soc.}, \bold{81}, 158--171.
}

\author{Alec Stephenson <alec_stephenson@hotmail.com>}


\seealso{\code{\link{rFrechet}},
  \code{\link{rGumbel}}, \code{\link{rRevWeibull}}}

\examples{
dGenExtrVal(2:4, 1, 0.5, 0.8)
pGenExtrVal(2:4, 1, 0.5, 0.8)
qGenExtrVal(seq(0.9, 0.6, -0.1), 2, 0.5, 0.8)
rGenExtrVal(6, 1, 0.5, 0.8)
p <- (1:9)/10
pGenExtrVal(qGenExtrVal(p, 1, 2, 0.8), 1, 2, 0.8)
## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
}
\keyword{distribution}

