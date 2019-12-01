\name{SpreadOut}
\alias{SpreadOut}

\title{Spread Out a Vector of Numbers To a Minimum Interval}
\description{
 Spread the numbers of a vector so that there is a minimum interval
 between any two numbers (in ascending or descending order). This is helpful when we want to place textboxes on a plot and ensure, that they do not mutually overlap.
}
\usage{
SpreadOut(x, mindist = NULL, cex = 1.0)
}
\arguments{
 \item{x}{a numeric vector which may contain \code{NA}s.}
 \item{mindist}{the minimum interval between any two values. If this is left to \code{NULL} (default) the function will check if a plot is open and then use 90\% of \code{\link{strheight}()}.}
  \item{cex}{numeric character expansion factor; multiplied by \code{\link{par}("cex")} yields the final character size; the default \code{NULL} is equivalent to \code{1}.}
}
\details{
 \code{SpreadOut()} starts at or near the middle of the vector and increases the
 intervals between the ordered values. \code{NA}s are preserved. \code{SpreadOut()}
 first tries to spread groups of values with intervals less than \code{mindist}
 out neatly away from the mean of the group. If this doesn't entirely succeed,
 a second pass that forces values away from the middle is performed.

 \code{SpreadOut()} can also be used to avoid overplotting of axis tick labels
 where they may be close together.
}
\value{
 On success, the spread out values. If there are less than two valid values, the original vector is returned.
}
\note{ This function is based on \code{plotrix::spreadout()} and has been
integrated here with some minor changes.
}
\seealso{\code{\link{strheight}()}}
\author{Jim Lemon <jim@bitwrit.com.au>\cr
some extensions Andri Signorell <andri@signorell.net>}
\examples{
SpreadOut(c(1, 3, 3, 3, 3, 5), 0.2)
SpreadOut(c(1, 2.5, 2.5, 3.5, 3.5, 5), 0.2)
SpreadOut(c(5, 2.5, 2.5, NA, 3.5, 1, 3.5, NA), 0.2)

# this will almost always invoke the brute force second pass
SpreadOut(rnorm(10), 0.5)
}
\keyword{misc}
