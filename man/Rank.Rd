\name{Rank}
\alias{Rank}
\title{Fast Sample Ranks}
\description{
The function \code{base::rank} has various weaknesses. Apart from the fact that it is not very fast, the option to calculate dense ranks is not implemented. Then, an argument for specifying the ranking direction is missing (assuming that this can be done with the ranking of the negative variables) and finally, multiple columns cannot be used in the case of ties for further ranking.
\cr The function \code{data.table::frankv} provides a more elaborated interface and convinces by very performant calculations and is \emph{much faster} than the original. 
It further accepts vectors, lists, \code{data.frame}s or \code{data.table}s as input. In addition to the \code{ties.method} possibilities provided by \code{base::rank}, it also provides \code{ties.method="dense"}. \cr The present function \code{Rank} is merely a somewhat customized parameterization of the \code{data.table} function. 
}

\usage{
Rank(..., decreasing = FALSE, na.last = TRUE,
     ties.method = c("average", "first", "last", "random",
                     "max", "min", "dense"))

}
\arguments{
  \item{...}{ A vector, or list with all its elements identical in length or \code{data.frame} or \code{data.table}. }
  
  \item{decreasing}{ An \code{logical} vector corresponding to ascending and descending order. \code{decreasing} is recycled to \code{length(...)}. }
  \item{na.last}{ Control treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed; if \code{"keep"} they are kept with rank \code{NA}. }
  \item{ties.method}{ A character string specifying how ties are treated, see \code{Details}. }
}
\details{
  To be consistent with other \code{data.table} operations, \code{NA}s are considered identical to other \code{NA}s (and \code{NaN}s to other \code{NaN}s), unlike \code{base::rank}. Therefore, for \code{na.last=TRUE} and \code{na.last=FALSE}, \code{NA}s (and \code{NaN}s) are given identical ranks, unlike \code{\link[base]{rank}}.

  \code{Rank} is not limited to vectors. It accepts \code{data.table}s (and \code{list}s and \code{data.frame}s) as well. It accepts unquoted column names (with names preceded with a \code{-} sign for descending order, even on character vectors), for e.g., \code{Rank(DT, a, -b, c, ties.method="first")} where \code{a,b,c} are columns in \code{DT}. 

  In addition to the \code{ties.method} values possible using base's \code{\link[base]{rank}}, it also provides another additional argument \code{"dense"}.
Dense ranks are consecutive integers beginning with 1. No ranks are skipped if there are ranks with multiple items. So the largest rank value is the number of unique values of x. See examples.

  Like \code{\link{forder}}, sorting is done in "C-locale"; in particular, this may affect how capital/lowercase letters are ranked. See Details on \code{forder} for more.

  \code{bit64::integer64} type is also supported.

}

\value{
  A numeric vector of length equal to \code{NROW(x)} (unless \code{na.last = NA}, when missing values are removed). The vector is of integer type unless \code{ties.method = "average"} when it is of double type (irrespective of ties).
}

\examples{
# on vectors
x <- c(4, 1, 4, NA, 1, NA, 4)
# NAs are considered identical (unlike base R)
# default is average
Rank(x) # na.last=TRUE
Rank(x, na.last=FALSE)

# ties.method = min
Rank(x, ties.method="min")
# ties.method = dense
Rank(x, ties.method="dense")

# on data.frame, using both columns
d.set <- data.frame(x, y=c(1, 1, 1, 0, NA, 0, 2))
Rank(d.set, na.last="keep")
Rank(d.set, ties.method="dense", na.last=NA)

# decreasing argument
Rank(d.set, decreasing=c(FALSE, TRUE), ties.method="first")
}
\seealso{
\code{\link{frankv}}, \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{setorder}}
}
\keyword{ data }
