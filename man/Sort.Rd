\name{Sort}
\alias{Sort}
\alias{Sort.default}
\alias{Sort.data.frame}
\alias{Sort.matrix}
\alias{Sort.table}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sort a Vector, a Matrix, a Table or a Data.frame
%%  ~~function to do ... ~~
}
\description{Sort a vector, a matrix, a table or a data.frame. The base sort function does not have an interface for classes other than vectors and coerces the whole world to a vector. This means you get a sorted vector as result while passing a matrix to \code{sort}.\cr
\code{Sort} wraps the base sort function and adds an interface for sorting the rows of the named 2-dimensional data structures by the order of one or more of its columns.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sort(x, ...)

\method{Sort}{default}(x, ...)
\method{Sort}{matrix}(x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...)
\method{Sort}{table}(x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...)
\method{Sort}{data.frame}(x, ord = NULL, decreasing = FALSE,
                factorsAsCharacter = TRUE, na.last = TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric, complex. character or logical vector, a factor, a table or a data.frame to be sorted. }
  \item{decreasing}{logical. Should the sort be increasing or decreasing? }
  %\item{bycol}{data.frame only. A vector with the position or the names of columns to be sorted after.}
  \item{factorsAsCharacter}{logical. Should factors be sorted by the alphabetic order of their labels or by the order or their levels.
     Default is \code{TRUE} (by labels).}

  \item{ord}{vector of integers or columnames. Defines the columns in a table, in a matrix or in a data.frame to be sorted for. \cr
    0 means row.names, 1:n the columns and n+1 the marginal sum. See examples.}
  \item{na.last}{for controlling the treatment of \code{NAs}. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed (see \code{\link{order}}.)}

  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
The sort order for factors is the order of their levels (which is particularly appropriate for ordered factors),
and usually confusing for unordered factors, whose levels may be defined in the sequence in which they appear in the data (which normally is unordered).
}
\value{the sorted object.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{sort}}, \code{\link{order}}}

\examples{
d.frm <- d.pizza[1:10, c("driver","temperature","delivery_min")]

Sort(d.frm[,1])
# Sort follows the levels by default
levels(d.frm[,1])

Sort(x=d.frm, ord="driver", decreasing=FALSE)
# set factorsAsCharacter = TRUE, if alphabetical order is required
Sort(x=d.frm, ord="driver", decreasing=FALSE, factorsAsCharacter=TRUE)

Sort(x=d.frm, ord=c("driver","delivery_min"), factorsAsCharacter = TRUE)
Sort(x=d.frm, ord=c("driver","delivery_min"), factorsAsCharacter = FALSE)

Sort(x=d.frm, ord=c("driver","delivery_min"), decreasing=c(FALSE, TRUE),
  factorsAsCharacter = FALSE)

# Sorting tables
tab <- table(d.pizza$driver, d.pizza$area)

Sort(x=tab, ord=c(0,2), decreasing=c(TRUE, FALSE))
Sort(x=tab, ord=2, decreasing=TRUE)

# partial matching ok:
Sort(tab, o=1, d=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

