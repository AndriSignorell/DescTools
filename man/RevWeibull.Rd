\name{RevWeibull}
\alias{dRevWeibull}
\alias{pRevWeibull}
\alias{qRevWeibull}
\alias{rRevWeibull}
\alias{dNegWeibull}
\alias{pNegWeibull}
\alias{qNegWeibull}
\alias{rNegWeibull}
\title{The Reverse Weibull Distribution}
\description{
  Density function, distribution function, quantile function and
  random generation for the reverse (or negative) Weibull
  distribution with location, scale and shape parameters.
}
\usage{
dRevWeibull(x, loc=0, scale=1, shape=1, log = FALSE)
pRevWeibull(q, loc=0, scale=1, shape=1, lower.tail = TRUE)
qRevWeibull(p, loc=0, scale=1, shape=1, lower.tail = TRUE)
rRevWeibull(n, loc=0, scale=1, shape=1)

dNegWeibull(x, loc=0, scale=1, shape=1, log = FALSE)
pNegWeibull(q, loc=0, scale=1, shape=1, lower.tail = TRUE)
qNegWeibull(p, loc=0, scale=1, shape=1, lower.tail = TRUE)
rNegWeibull(n, loc=0, scale=1, shape=1)
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{loc, scale, shape}{Location, scale and shape parameters (can be
    given as vectors).}
  \item{log}{Logical; if \code{TRUE}, the log density is returned.}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities
    are P[X <= x], otherwise, P[X > x]}
}
\details{
  The reverse (or negative) Weibull distribution function with parameters
  \eqn{loc = a}, \eqn{scale = b} and
  \eqn{shape = s} is
  \deqn{G(z) = \exp\left\{-\left[-\left(\frac{z-a}{b}\right)
    \right]^s\right\}}{G(x) = exp{-[-(z-a)/b]^s}}
  for \eqn{z < a} and one otherwise, where \eqn{b > 0} and
  \eqn{s > 0}.
}
\note{
  Within extreme value theory the reverse Weibull distibution (also
  known as the negative Weibull distribution) is often referred to
  as the Weibull distribution.
  We make a distinction to avoid confusion with the three-parameter
  distribution used in survival analysis, which is related by a
  change of sign to the distribution given above.
}
\value{
  \code{dRevWeibull} and \code{dNegWeibull} give the density function,
  \code{pRevWeibull} and \code{pNegWeibull} give the distribution function,
  \code{qRevWeibull} and \code{qNegWeibull} give the quantile function,
  \code{rRevWeibull} and \code{rNegWeibull} generate random deviates.
}

\seealso{\code{\link{rFrechet}}, \code{\link{rGenExtrVal}}, \code{\link{rGumbel}}}

\author{Alec Stephenson <alec_stephenson@hotmail.com>}

\examples{
dRevWeibull(-5:-3, -1, 0.5, 0.8)
pRevWeibull(-5:-3, -1, 0.5, 0.8)
qRevWeibull(seq(0.9, 0.6, -0.1), 2, 0.5, 0.8)
rRevWeibull(6, -1, 0.5, 0.8)
p <- (1:9)/10
pRevWeibull(qRevWeibull(p, -1, 2, 0.8), -1, 2, 0.8)
## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
}
\keyword{distribution}




