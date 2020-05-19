\name{Coalesce}
\alias{Coalesce}

\title{Return the First Element Not Being NA
}
\description{Return the first element of a vector, not being NA.
}
\usage{
Coalesce(\dots, method = c("is.na", "is.null", "is.finite"), flatten = TRUE)
}
\arguments{
  \item{\dots}{the elements to be evaluated. This can either be a single vector, several
  vectors of same length, a matrix, a data.frame or a list of vectors (of same length). See examples.}
  \item{method}{one out of \code{"is.na"} (default), \code{"is.null"}  or \code{"is.finite"}. The \code{"is.na"} option allows \code{Inf} values to
   be in the result, the second one eliminates them.
}
\item{flatten}{logical, defines whether lists are going to be flattened (default \code{TRUE}).}
}
\details{
If several vectors are supplied, the evaluation will be elementwise, resp. rowwise if x is a data.frame or a matrix. The first element of the result
is the first non \code{NA} element of the first elements of all the arguments, the second element of
the result is the one of the second elements of all the arguments and so on. \cr
Shorter inputs (of non-zero length) are NOT recycled. The function will bark, if multiple vectors do not all have the same dimension.\cr
The idea is borrowed from SQL. Might sometimes be useful when preparing data in R instead of in SQL.
}
\value{return a single vector of the first non \code{NA} element(s) of the given data structure.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{is.na}}, \code{\link{is.finite}}
}


\examples{
Coalesce(c(NA, NA, NA, 5, 3))
Coalesce(c(NA, NULL, "a"))
Coalesce(NULL, 5, 3)

d.frm <- data.frame(matrix(c(
  1, 2, NA, 4,
  NA, NA, 3, 1,
  NaN, 2, 3, 1,
  NA, Inf, 1, 1), nrow=4, byrow=TRUE)
)

Coalesce(d.frm)
Coalesce(as.matrix(d.frm))
Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4, method="is.finite")
Coalesce(list(d.frm[,1], d.frm[,2]))

# returns the first finite element
Coalesce(d.frm, method="is.finite")

# with characters (take care, factors won't work!)
# is.finite does not make sense here...
d.frm <- data.frame(matrix(c(
  "a", "b", NA, "4",
  NA, NA, "g", "m",
  NA_character_,"hfdg", "rr", "m",
  NA, Inf, 1, 1), nrow=4, byrow=TRUE)
, stringsAsFactors = FALSE)

Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
Coalesce(d.frm)
Coalesce(as.list(d.frm))
}
\keyword{ manip }
