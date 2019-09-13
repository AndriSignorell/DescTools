\name{Gompertz}
\alias{Gompertz}
\alias{dGompertz}
\alias{pGompertz}
\alias{qGompertz}
\alias{rGompertz}
\title{The Gompertz distribution}
\usage{
dGompertz(x, shape, rate = 1, log = FALSE)
pGompertz(q, shape, rate = 1, lower.tail = TRUE, log.p = FALSE)
qGompertz(p, shape, rate = 1, lower.tail = TRUE, log.p = FALSE)
rGompertz(n, shape = 1, rate = 1)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{shape, rate}{vector of shape and rate parameters.}

\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}

\item{lower.tail}{logical; if TRUE (default), probabilities are \eqn{P(X
\le x)}{P(X <= x)}, otherwise, \eqn{P(X > x)}{P(X > x)}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations. If \code{length(n) > 1}, the length is
taken to be the number required.}
}
\value{
\code{dGompertz} gives the density, \code{pGompertz} gives the
distribution function, \code{qGompertz} gives the quantile function,
and \code{rGompertz} generates random deviates.
}
\description{
Density, distribution function, quantile function and random
generation for the Gompertz distribution with unrestricted shape.
}
\details{
The Gompertz distribution with \code{shape} parameter \eqn{a} and
\code{rate} parameter \eqn{b}{b} has probability density function

\deqn{f(x | a, b) = be^{ax}\exp(-b/a (e^{ax} - 1))}{f(x | a, b) = b exp(ax)
exp(-b/a (exp(ax) - 1))}

For \eqn{a=0} the Gompertz is equivalent to the exponential distribution
with constant hazard and rate \eqn{b}.

The probability distribution function is \deqn{F(x | a, b) = 1 - \exp(-b/a
(e^{ax} - 1))}{F(x | a, b) = 1 - exp(-b/a (exp(ax) - 1))}

Thus if \eqn{a} is negative, letting \eqn{x} tend to infinity shows that
there is a non-zero probability \eqn{1 - \exp(b/a)}{1 - exp(b/a)} of living
forever.  On these occasions \code{qGompertz} and \code{rGompertz} will
return \code{Inf}.
}
\note{
Some implementations of the Gompertz restrict \eqn{a} to be strictly
positive, which ensures that the probability of survival decreases to zero
as \eqn{x} increases to infinity.  The more flexible implementation given
here is consistent with \code{streg} in Stata.

The functions \code{dGompertz} and similar available in the
package \pkg{eha} label the parameters the other way round, so that what is
called the \code{shape} there is called the \code{rate} here, and what is
called \code{1 / scale} there is called the \code{shape} here. The
terminology here is consistent with the exponential \code{\link{dexp}} and
Weibull \code{\link{dweibull}} distributions in R.
}
\author{
Christopher Jackson <chris.jackson@mrc-bsu.cam.ac.uk>
}
\references{
Stata Press (2007) Stata release 10 manual: Survival analysis
and epidemiological tables.
}
\seealso{
\code{\link{dexp}}
}
\keyword{distribution}

