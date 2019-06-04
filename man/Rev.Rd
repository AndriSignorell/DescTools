\name{Rev}
\alias{Rev}
\alias{Rev.default}
\alias{Rev.matrix}
\alias{Rev.table}
\alias{Rev.data.frame}
% \alias{Rev.data.frame}

\title{Reverse Elements of a Vector or the Rows/Columns of Matrices, Tables or Data.frames
}
\description{\code{Rev} provides a reversed version of its argument.
It wraps the base function \code{rev} and provides an additional method for matrices and higher dimensional tables, such as to reverse the order of rows and columns.
}
\usage{
Rev(x, ...)

\method{Rev}{matrix}(x, margin, \dots)

\method{Rev}{table}(x, margin, \dots)

\method{Rev}{data.frame}(x, margin, \dots)

}
\arguments{
  \item{x}{a vector, a matrix or a higher dimensional table to be reversed.
}
  \item{margin}{vector of dimensions which to be reversed (1 for rows, 2 for columns, etc.).
}
  \item{\dots}{the dots are passed to the matrix, resp. table interface.
}
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{rev}}, \code{\link{order}}, \code{\link{sort}}, \code{\link{seq}}
}
\examples{
tab <- matrix( c( 1, 11, 111,
                  2, 22, 222,
                  3, 33, 333), byrow=TRUE, nrow=3)

Rev(tab, margin=1)
Rev(tab, margin=2)

# reverse both dimensions
Rev(tab, margin=c(1,2))
}
\keyword{ manip }
