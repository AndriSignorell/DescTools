\name{CrossN}
\alias{CrossN}
\title{n-dimensional Vector Cross Product}
\description{
  Vector cross product of \code{n-1} vectors in n-dimensional space
}
\usage{
CrossN(A)
}
\arguments{
  \item{A}{matrix of size \code{(n-1) x n} where \code{n >= 2}.}
}
\details{
  The rows of the matrix \code{A} are taken as\code{(n-1)} vectors in
  \code{n}-dimensional space. The cross product generates a vector in this
  space that is orthogonal to all these rows in \code{A} and its length is
  the volume of the geometric hypercube spanned by the vectors.
}
\value{
  a vector of length \code{n}
}
\note{
  The `scalar triple product' in \eqn{R^3} can be defined as

  \code{spatproduct <- function(a, b, c) Dot(a, CrossN(b, c))}

  It represents the volume of the parallelepiped spanned by the three vectors.
}
\seealso{
\code{\link{Cross}}, \code{\link{Dot}}
}
\author{Hans W. Borchers <hwborchers@googlemail.com>}
\examples{
A <- matrix(c(1,0,0, 0,1,0), nrow=2, ncol=3, byrow=TRUE)
CrossN(A)  #=> 0 0 1

x <- c(1.0, 0.0, 0.0)
y <- c(1.0, 0.5, 0.0)
z <- c(0.0, 0.0, 1.0)
identical(Dot(x, CrossN(rbind(y, z))), det(rbind(x, y, z)))
}
\keyword{ array }
