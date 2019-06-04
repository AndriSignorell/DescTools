\name{Primes}
\alias{Primes}
\title{Find All Primes Less Than n}
\description{
    Find all prime numbers aka \sQuote{primes} less than \eqn{n}.

    Uses an obvious sieve method and some care, working with
    logical and integers to be quite fast.
}
\usage{
Primes(n)
}
\arguments{
  \item{n}{a (typically positive integer) number.}
}
\details{
  As the function only uses \code{\link{max}(n)}, \code{n} can also be a
  \emph{vector} of numbers.
}
\value{
  numeric vector of all prime numbers \eqn{\le n}{<= n}.
}
% \references{
% ...
% }
\note{ This function was previously published in the package \pkg{sfsmisc} as \code{\link[sfsmisc]{primes}}
  and has been integrated here without logical changes.
}
\author{
  Bill Venables (\eqn{\le n}{<= 2001}); Martin Maechler gained another 40\% speed,
  working with logicals and integers.
}
\seealso{
 \code{\link{Factorize}},  \code{\link{GCD}},  \code{\link{LCM}}, \code{\link{IsPrime}}
}
\examples{
(p1 <- Primes(100))
system.time(p1k <- Primes(1000)) # still lightning ..

stopifnot(length(p1k) == 168)

}
\keyword{math}
\keyword{arith}


