\name{Rev}
\alias{Rev}
\alias{Rev.default}
\alias{Rev.matrix}
\alias{Rev.table}
\alias{Rev.array}
\alias{Rev.data.frame}
% \alias{Rev.data.frame}

\title{Reverse Elements of a Vector, a Matrix, a Table, an Array or a Data.frame
}
\description{\code{Rev} provides a reversed version of its argument.
Unlike the basic function, it does in higher-dimensional structures such as matrices not reverse the elements, but the order of the rows and/or columns. It further offers additional interfaces for higher dimensional arrays or tables.
}
\usage{
Rev(x, ...)

\method{Rev}{matrix}(x, margin, \dots)

\method{Rev}{table}(x, margin, \dots)

\method{Rev}{array}(x, margin, \dots)

\method{Rev}{data.frame}(x, margin, \dots)
}
\arguments{
  \item{x}{a vector, a matrix or a higher dimensional table to be reversed.
}
  \item{margin}{vector of dimensions which to be reversed (1 for rows, 2 for columns, etc.). If not defined, all dimensions will be reverted.
}
  \item{\dots}{the dots are passed to the array interface.
}
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{rev}}, \code{\link{order}}, \code{\link{sort}}, \code{\link{seq}}
}
\examples{
tab <- matrix(c(1, 11, 111,
                2, 22, 222,
                3, 33, 333), 
              byrow=TRUE, nrow=3,
              dimnames=list(mar1=1:3, mar2=c("a","b","c")))

Rev(tab, margin=1)
Rev(tab, margin=2)

# reverse both dimensions
Rev(tab, margin=c(1, 2))

t(tab)

# reverse 3dimensional array
aa <- Abind(tab, 2 * tab, along=3)
dimnames(aa)[[3]] <- c("A","Z")

# reverse rows
Rev(aa, 1)
# reverse columns
Rev(aa, 2)
# reverse 3th dimension
Rev(aa, 3)

# reverse all dimensions
Rev(aa)
# same as
Rev(aa, margin=(1:3))
}
\keyword{ manip }
