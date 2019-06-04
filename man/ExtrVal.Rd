\name{ExtrVal}
\alias{dExtrVal}
\alias{pExtrVal}
\alias{qExtrVal}
\alias{rExtrVal}
\title{Distributions of Maxima and Minima}
\description{
  Density function, distribution function, quantile function and
  random generation for the maximum/minimum of a given number of
  independent variables from a specified distribution.
}
\usage{
dExtrVal(x, densfun, distnfun, \dots, distn, mlen = 1, largest = TRUE,
    log = FALSE)
pExtrVal(q, distnfun, \dots, distn, mlen = 1, largest = TRUE,
    lower.tail = TRUE)
qExtrVal(p, quantfun, \dots, distn, mlen = 1, largest = TRUE,
    lower.tail = TRUE)
rExtrVal(n, quantfun, \dots, distn, mlen = 1, largest = TRUE)
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{densfun, distnfun, quantfun}{Density, distribution and
    quantile function of the specified distribution. The density
    function must have a \code{log} argument (a simple wrapper
    can always be constructed to achieve this).}
  \item{\dots}{Parameters of the specified distribution.}
  \item{distn}{A character string, optionally given as an
    alternative to \code{densfun}, \code{distnfun} and \code{quantfun}
    such that the density, distribution and quantile functions are
    formed upon the addition of the prefixes \code{d}, \code{p} and
    \code{q} respectively.}
  \item{mlen}{The number of independent variables.}
  \item{largest}{Logical; if \code{TRUE} (default) use maxima,
    otherwise minima.}
  \item{log}{Logical; if \code{TRUE}, the log density is returned.}
  \item{lower.tail}{Logical; if \code{TRUE} (default) probabilities
    are P[X <= x], otherwise  P[X > x].}
}
\value{
  \code{dExtrVal} gives the density function, \code{pExtrVal} gives the
  distribution function and \code{qExtrVal} gives the quantile function
  of the maximum/minimum of \code{mlen} independent variables from
  a specified distibution. \code{rExtrVal} generates random deviates.
}

\seealso{\code{\link{rGenExtrVal}}, \code{\link{rOrder}}}

\author{Alec Stephenson <alec_stephenson@hotmail.com>}

\examples{
dExtrVal(2:4, dnorm, pnorm, mean = 0.5, sd = 1.2, mlen = 5)
dExtrVal(2:4, distn = "norm", mean = 0.5, sd = 1.2, mlen = 5)
dExtrVal(2:4, distn = "exp", mlen = 2, largest = FALSE)
pExtrVal(2:4, distn = "exp", rate = 1.2, mlen = 2)
qExtrVal(seq(0.9, 0.6, -0.1), distn = "exp", rate = 1.2, mlen = 2)
rExtrVal(5, qgamma, shape = 1, mlen = 10)
p <- (1:9)/10
pexp(qExtrVal(p, distn = "exp", rate = 1.2, mlen = 1), rate = 1.2)
## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
}
\keyword{distribution}


