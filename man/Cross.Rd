\name{Cross}
\alias{Cross}
\title{Vector Cross Product}
\description{
  Vector or cross product
}
\usage{
  Cross(x, y)
}
\arguments{
  \item{x}{numeric vector or matrix}
  \item{y}{numeric vector or matrix}
}
\details{
  Computes the cross (or: vector) product of vectors in 3 dimensions.
  In case of matrices it takes the first dimension of length 3 and
  computes the cross product between corresponding columns or rows.

  The more general cross product of \code{n-1} vectors in n-dimensional
  space is realized as \code{CrossN}.
}
\value{
  3-dim. vector if \code{x} and \code{<} are vectors, a matrix of
  3-dim. vectors if \code{x} and \code{y} are matrices themselves.
}
\author{Hans W. Borchers <hwborchers@googlemail.com>}

\seealso{
  \code{\link{Dot}}, \code{\link{CrossN}}
}
\examples{
Cross(c(1, 2, 3), c(4, 5, 6))  # -3  6 -3

# Triple product can be calculated as:
va <- c(1, 2, 3)
vb <- c(4, 3, 0)
vc <- c(5, 1, 1)

Dot(va, Cross(vb, vc))
}
\keyword{ array }
