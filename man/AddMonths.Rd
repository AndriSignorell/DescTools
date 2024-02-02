\name{AddMonths}
\alias{AddMonths}
\title{Add a Month to a Date
}
\description{Clueless adding numbers of months to a date will in some cases lead to invalid dates, think of e.g. 2012-01-30 + 1 month. \cr AddMonths ensures that the result is always a valid date, e.g.
\code{as.Date("2013-01-31") + 1 month} will be \code{"2013-02-28"}. If number \code{n} is negative, the months will be subtracted.
}
\usage{
AddMonths(x, n, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a Date object (or something which can be coerced by \code{\link{as.Date}}(x, ...) to such an object)
  to which a number of months has to be added.
%%     ~~Describe \code{x} here~~
}
  \item{n}{the number of months to be added. If n is negative the months will be subtracted.
%%     ~~Describe \code{n} here~~
}
  \item{\dots}{the dots are passed to \code{\link{as.Date}}, e.g. for supplying \code{origin}.
%%     ~~Describe \code{ceiling} here~~
}
}
\details{All parameters will be recyled if necessary.
}
\value{a vector of class \code{Date} with the same dimension as \code{x}, containing the transformed dates.
}
\references{
Thanks to Antonio: \url{https://stackoverflow.com/questions/14169620/add-a-month-to-a-date}
}
\author{Andri Signorell <andri@signorell.net>, based on code by Roland Rapold and Antonio
}

\seealso{\code{\link{as.ym}}; Date functions: \code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# characters will be coerced to Date
AddMonths("2013-01-31", 1)

# negative n
AddMonths(as.Date("2013-03-31"), -1)

# Arguments will be recycled
# (with warning if the longer is not a multiple of length of shorter)
AddMonths(c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"), c(1,-1))


x <- as.POSIXct(c("2015-01-31", "2015-08-31"))
n <- c(1, 3)
AddMonths(x, n)

# mind the origin if x supplied as numeric ...
x <- as.numeric(as.Date(x))
AddMonths(x, n, origin=as.Date("1970-01-01"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
