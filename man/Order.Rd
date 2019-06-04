\name{Order}
\alias{dOrder}
\alias{pOrder}
\alias{rOrder}
\title{Distributions of Order Statistics}
\description{
  Density function, distribution function and random generation for
  a selected Order statistic of a given number of independent variables
  from a specified distribution.
}
\usage{
dOrder(x, densfun, distnfun, \dots, distn, mlen = 1, j = 1,
      largest = TRUE, log = FALSE)
pOrder(q, distnfun, \dots, distn, mlen = 1, j = 1, largest = TRUE,
      lower.tail = TRUE)
rOrder(n, quantfun, \dots, distn, mlen = 1, j = 1, largest = TRUE)
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{n}{Number of observations.}
  \item{densfun, distnfun, quantfun}{Density, distribution and
    quantile function of the specified distribution. The density
    function must have a \code{log} argument (a simple wrapper
    can always be constructed to achieve this).}
  \item{\dots}{Parameters of the specified distribution.}
  \item{distn}{A character string, optionally specified as an
    alternative to \code{densfun}, \code{distnfun} and \code{quantfun}
    such that the density, distribution and quantile functions are
    formed upon the addition of the prefixes \code{d}, \code{p} and
    \code{q} respectively.}
  \item{mlen}{The number of independent variables.}
  \item{j}{The Order statistic, taken as the \code{j}th largest
    (default) or smallest of \code{mlen}, according to the value of
    \code{largest}.}
  \item{largest}{Logical; if \code{TRUE} (default) use the \code{j}th
    largest Order statistic, otherwise use the \code{j}th smallest.}
  \item{log}{Logical; if \code{TRUE}, the log density is returned.}
  \item{lower.tail}{Logical; if \code{TRUE} (default) probabilities
    are P[X <= x], otherwise  P[X > x].}
}
\value{
  \code{dOrder} gives the density function, \code{pOrder} gives the
  distribution function and \code{qOrder} gives the quantile function
  of a selected Order statistic from a sample of size \code{mlen},
  from a specified distibution. \code{rOrder} generates random deviates.
}

\seealso{\code{\link{rExtrVal}}, \code{\link{rGenExtrVal}}}

\author{Alec Stephenson <alec_stephenson@hotmail.com>}

\examples{
dOrder(2:4, dnorm, pnorm, mean = 0.5, sd = 1.2, mlen = 5, j = 2)
dOrder(2:4, distn = "norm", mean = 0.5, sd = 1.2, mlen = 5, j = 2)
dOrder(2:4, distn = "exp", mlen = 2, j = 2)
pOrder(2:4, distn = "exp", rate = 1.2, mlen = 2, j = 2)
rOrder(5, qgamma, shape = 1, mlen = 10, j = 2)
}
\keyword{distribution}

