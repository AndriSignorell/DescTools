\name{Some}
\alias{Some}
\alias{Some.default}
\alias{Some.data.frame}
\alias{Some.matrix}
\title{
  Return Some Randomly Chosen Elements of an Object
}
\description{
For displaying the first and last elements of an object there are the functions \code{head} and \code{tail}. Sometimes one might want to see more randomly scattered elements. This function returns some random parts of a vector, matrix or a data frame. The order of the elements within the object will be preserved.
}
\usage{
Some(x, n = 6L, \dots)
\method{Some}{default}(x, n = 6L, \dots)
\method{Some}{data.frame}(x, n = 6L, \dots)
\method{Some}{matrix}(x, n = 6L, addrownums = TRUE, \dots)
}
\arguments{
  \item{x}{an object}
  \item{n}{a single integer. If positive, size for the resulting
    object: number of elements for a vector (including lists), rows for
    a matrix or data frame or lines for a function. If negative, all but
    the \code{n} last/first number of elements of \code{x}.}
  \item{addrownums}{if there are no row names, create them from the row
    numbers.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  For matrices, 2-dim tables and data frames, \code{Some()} returns
  some \code{n} rows when \code{n > 0} or all but the
  some \code{n} rows when \code{n < 0}.  \code{Some.matrix()} is not exported (unlike \code{head.matrix}).

  If a matrix has no row names, then \code{Some()} will add row names of
  the form \code{"[n,]"} to the result, so that it looks similar to the
  last lines of \code{x} when printed.  Setting \code{addrownums =
    FALSE} suppresses this behaviour.

    I desisted from implementing interfaces for tables, ftables and functions, as this would not make much sense.
}
\value{
  An object (usually) like \code{x} but generally smaller.
}
\author{
  Andri Signorell, basically copying and just slightly modifying Patrick Burns and R-Core code.
}

\seealso{\code{\link{head}}}

\examples{
Some(letters)
Some(letters, n = -6L)

Some(freeny.x, n = 10L)
Some(freeny.y)
}
\keyword{ manip }
