\name{as.ym}
\alias{as.ym}
\alias{as.Date.ym}
\alias{AddMonths.ym}

\title{A Class for Dealing with the Yearmonth Format
}
\description{
The representation of year and month information in YYYYYMM format as an integer is often handy and a useful and efficient data structure. Adding a number of months to such a date is not quite catchy, however, since the date structure is to be retained. For example, 201201 - 2 [months] is expected to result in 201111 instead of 201199. AddMonthsYM does this job.
}
\usage{

as.ym(x)
\method{as.Date}{ym}(x, d = 1, ...)

\method{AddMonths}{ym}(x, n, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of integers, representing the dates in the format YYYYMM, to which a number of months has to be added. YYYY must lie in the range of 1000-3000, MM in 1-12.
}
  \item{d}{the day to be used for converting a yearmonth to a date. Default is 1. 
%%     ~~Describe \code{n} here~~
}
  \item{n}{the number of months to be added. If n is negative the months will be subtracted.
%%     ~~Describe \code{n} here~~
}
\item{\dots}{further arguments (not used here).}
}
\details{All parameters will be recyled if necessary. The therefore used function \code{\link{mapply}} will display a warning, if the longer argument is not a multiple of the length of the shorter one.
}
\value{a vector of class \code{integer} with the same dimension as x, containing the transformed dates.
}
\author{Andri Signorell <andri@signorell.net>, originally based on code by Roland Rapold
}

\seealso{\code{\link{AddMonths}}; Date functions, like \code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Month(as.ym(202408))
Year(as.ym(202408))

Year(as.Date("2024-12-05"))
Year(as.ym(202412))

Month(as.Date("2024-12-05"), fmt = "mm")
Month(as.ym(202412), fmt="mm")

AddMonths(201511, 5)

AddMonths(c(201511, 201302), c(5, 15))
AddMonths(c(201511, 201302), c(5, -4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
