\name{matpow}
\alias{\%^\%}
\alias{matpow}
\title{Matrix Power}
\description{
  Compute the \eqn{k}-th power of a matrix. Whereas \code{x^k} computes
  \emph{element wise} powers, \code{x \%^\% k} corresponds to \eqn{k -
  1} matrix multiplications, \code{x \%*\% x \%*\% ... \%*\% x}.
}
\usage{
x \%^\% k
}
\arguments{
  \item{x}{a square \code{\link{matrix}}.}
  \item{k}{an integer, \eqn{k \ge 0}{k >= 0}.}
}
\details{
  Argument \eqn{k} is coerced to integer using \code{\link{as.integer}}.

  The algorithm uses \eqn{O(log_2(k))}{O(log2(k))} matrix
  multiplications.
}
\value{
  A matrix of the same dimension as \code{x}.
}
\author{
  Based on an R-help posting of Vicente Canto Casasola, and
  Vincent Goulet's C implementation in \pkg{actuar}.
}
\note{
  If you think you need \code{x^k} for \eqn{k < 0}, then consider
  instead \code{solve(x \%^\% (-k))}.
}
\seealso{
  \code{\link{\%*\%}} for matrix multiplication.
}
\examples{
A <- cbind(1, 2 * diag(3)[,-1])
A
A \%^\% 2
stopifnot(identical(A, A \%^\% 1),
          A \%^\% 2 == A \%*\% A)
}
\keyword{array}
\keyword{arith}
