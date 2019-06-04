\name{identify.formula}
\alias{identify.formula}
\title{Identify Points In a Plot Using a Formula}
\usage{
  \method{identify}{formula} (formula, data, subset, na.action, \dots)
}
\arguments{
    \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}

\item{data}{The data frame from which the formula should
  be evaluated.}

  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to
\code{getOption("na.action")}.}

  \item{\dots}{Other arguments to be passed to
  \code{\link{identify}}.}
}
\value{
  If \code{pos} is \code{FALSE}, an integer vector containing the
  indices of the identified points, in the order they were identified.
  If \code{pos} is \code{TRUE}, a list containing a component
  \code{ind}, indicating which points were identified and a component
  \code{pos}, indicating where the labels were placed relative to the
  identified points (1=below, 2=left, 3=above, 4=right and 0=no offset,
  used if \code{atpen = TRUE}).
}

\author{Derek Ogle <dogle@northland.edu>}
\description{
  The function \code{\link{identify}} reads the position of the graphics pointer when
  the (first) mouse button is pressed. It then searches the
  coordinates given in x and y for the point closest to the
  pointer. If this point is close enough to the pointer,
  its index will be returned as part of the value of the
  call.
}
\details{
  This function is meant to make it easier to call
  \code{\link{identify}} after \code{\link{plot}} has been called
  using a formula and the \code{data} argument.

  A two dimensional plot must be active and the vectors in
  \code{x} and data frame in \code{data} must correspond to
  the x- and y-axes and the data of the plot.
}
\examples{
\dontrun{
## Copy and try in an interactive R session
plot(dist ~ speed, data = cars, subset = speed < 17)
identify(dist ~ speed, data = cars, subset = speed < 17)
}
}
\seealso{
  \code{identify}, \code{\link{locator}}, \code{\link{text}}\cr
  \url{http://www.rforge.net/NCStats/files/}
}
\keyword{hplot}

