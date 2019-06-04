\name{IsEuclid}
\alias{IsEuclid}
\alias{summary.dist}
\title{Is a Distance Matrix Euclidean?}
\description{
Confirmation of the Euclidean nature of a distance matrix by the Gower's theorem.\cr
\code{IsEuclid} is used in \code{summary.dist}.\cr
}
\usage{
IsEuclid(distmat, plot = FALSE, print = FALSE, tol = 1e-07)
}
\arguments{
  \item{distmat}{an object of class 'dist'}
  \item{plot}{a logical value indicating whether the eigenvalues bar plot of the matrix of the term \eqn{-\frac{1}{2} {d_{ij}^2}}{-1/2 dij^2} centred by rows and columns should be diplayed}
  \item{print}{a logical value indicating whether the eigenvalues of the matrix of the term \eqn{-\frac{1}{2} {d_{ij}^2}}{-1/2 dij^2} centred by rows and columns should be printed}
  \item{tol}{a tolerance threshold : an eigenvalue is considered positive if it is larger than \code{-tol*lambda1} where \code{lambda1} is the largest eigenvalue.}
}
\value{
returns a logical value indicating if all the eigenvalues are positive or equal to zero
}
\references{Gower, J.C. and Legendre, P. (1986) Metric and Euclidean properties of dissimilarity coefficients. \emph{Journal of Classification}, \bold{3}, 5--48.
}
\author{
Daniel Chessel  \cr
Stephane Dray \email{dray@biomserv.univ-lyon1.fr}
}
\examples{
w <- matrix(runif(10000), 100, 100)
w <- dist(w)
summary(w)
IsEuclid (w) # TRUE
}
\keyword{arith}
