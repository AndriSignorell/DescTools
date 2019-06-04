\name{IsPrime}
\alias{IsPrime}
\title{IsPrime Property}
\description{
Returns for a vector or matrix of positive integers a logical object of the same dimension(s) containing \code{TRUE} for the elements that are prime and \code{FALSE} otherwise.
}
\usage{
IsPrime(x)
}
\arguments{
\item{x}{vector or matrix of nonnegative integers}
}
\details{
  Given a vector or a matrix of positive integers returns a vector of the same size
  of \code{FALSE} and \code{TRUE}. Use \code{which(IsPrime(1:21))} to get the positions.
}
\value{
logical vector
}
\author{Hans W. Borchers <hwborchers@googlemail.com>}

\seealso{
  \code{\link{Factorize}, \link{Primes}}
}
\examples{
x <- matrix(1:10, nrow=10, ncol=10, byrow=TRUE)
x * IsPrime(x)

# Find first prime number octett:
octett <- c(0, 2, 6, 8, 30, 32, 36, 38) - 19
while (TRUE) {
    octett <- octett + 210
    if (all(IsPrime(octett))) {
        cat(octett, "\n", sep="  ")
        break
    }
}
}
