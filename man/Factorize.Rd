\name{Factorize}
\alias{Factorize}
\title{Prime Factorization of Integers}
\description{
  Compute the prime factorization(s) of integer(s) \code{n}.

  % ## Purpose:  Prime factorization of integer(s) 'n'
  % ## -------------------------------------------------------------------------
  % ## Arguments: n vector of integers to Factorize (into prime numbers)
  % ##	--> needs 'prime.sieve'
  % ## >> Better would be: Define class 'primefactors' and "multiply" method
  % ##			 then use this function recursively only "small" factors
  % ## -------------------------------------------------------------------------
}
\usage{
Factorize(n)
}
\arguments{
  \item{n}{vector of integers to factorize.}
}
\details{
  works via \code{\link{Primes}}, currently in a cheap way, sub-optimal
  for large composite \eqn{n}.
%% FIXME: ALSO,
%% % ## >> Better would be: Define class 'primefactors' and "multiply" method
}
\value{
  A named \code{\link{list}} of the same length as \code{n},
  each element a 2-column matrix with column \code{"p"} the prime
  factors and column~\code{"m"} their respective exponents (or
  multiplities), i.e., for a prime number \code{n}, the resulting matrix
  is \code{cbind(p = n, m = 1)}.
}
\author{
Martin Maechler, Jan. 1996.
}
\seealso{
  \code{\link{GCD}},  \code{\link{LCM}}, \code{\link{Primes}},   \code{\link{IsPrime}},   \code{\link{Divisors}}

  For factorization of moderately or really large numbers, see the \pkg{gmp}
  package, and its \code{\link[gmp]{factorize}()} (which is ~20x faster!).
}
\examples{
 Factorize(47)
 Factorize(seq(101, 120, by=2))
}
\keyword{math}
\keyword{arith}

