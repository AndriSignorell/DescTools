\name{GenPareto}
\alias{dGenPareto}
\alias{pGenPareto}
\alias{qGenPareto}
\alias{rGenPareto}
\title{The Generalized Pareto Distribution}
\description{
  Density function, distribution function, quantile function and
  random generation for the generalized Pareto distribution (GenPareto)
  with location, scale and shape parameters.
}
\usage{
dGenPareto(x, loc=0, scale=1, shape=0, log = FALSE)
pGenPareto(q, loc=0, scale=1, shape=0, lower.tail = TRUE)
qGenPareto(p, loc=0, scale=1, shape=0, lower.tail = TRUE)
rGenPareto(n, loc=0, scale=1, shape=0)
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
  The generalized Pareto distribution function (Pickands, 1975) with
  parameters \eqn{loc = a}, \eqn{scale = b} and
  \eqn{shape = s} is
  \deqn{G(z) = 1 - \{1+s(z-a)/b\}^{-1/s}}{
    G(z) = 1 - {1+s(z-a)/b}^(-1/s)}
  for \eqn{1+s(z-a)/b > 0} and \eqn{z > a}, where \eqn{b > 0}.
  If \eqn{s = 0} the distribution is defined by continuity.
}
\value{
  \code{dGenPareto} gives the density function, \code{pGenPareto} gives the
  distribution function, \code{qGenPareto} gives the quantile function,
  and \code{rGenPareto} generates random deviates.
}
\references{
  Pickands, J. (1975)
  Statistical inference using Extreme Order statistics.
  \emph{Annals of Statistics}, \bold{3}, 119--131.
}

\seealso{\code{\link{rGenExtrVal}}}

\author{Alec Stephenson <alec_stephenson@hotmail.com>}

\examples{
dGenPareto(2:4, 1, 0.5, 0.8)
pGenPareto(2:4, 1, 0.5, 0.8)
qGenPareto(seq(0.9, 0.6, -0.1), 2, 0.5, 0.8)
rGenPareto(6, 1, 0.5, 0.8)
p <- (1:9)/10
pGenPareto(qGenPareto(p, 1, 2, 0.8), 1, 2, 0.8)
## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
}
\keyword{distribution}

