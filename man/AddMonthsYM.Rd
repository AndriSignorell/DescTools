\name{AddMonthsYM}
\alias{AddMonthsYM}
\title{Add a Month to a Date in YearMonth Format
}
\description{
The representation of year and month information in YYYYYMM format as an integer is sometimes a useful and efficient data structure. Adding a number of months to such a date is not quite catchy, however, since the date structure is to be retained. For example, 201201 - 2 [months] is expected to result in 201111 instead of 201199. AddMonthsYM does this job.
}
\usage{
AddMonthsYM(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of integers, representing the dates in the format YYYYMM, to which a number of months has to be added.
}
  \item{n}{the number of months to be added. If n is negative the months will be subtracted.
%%     ~~Describe \code{n} here~~
}
}
\details{All parameters will be recyled if necessary. The therefore used function \code{\link{mapply}} will display a warning, if the longer argument is not a multiple of the length of the shorter one.
}
\value{a vector of class \code{integer} with the same dimension as x, containing the transformed dates.
}
\author{Andri Signorell <andri@signorell.net>, based on code by Roland Rapold
}

\seealso{\code{\link{AddMonths}}; Date functions, like \code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
AddMonthsYM(201511, 5)

AddMonthsYM(c(201511, 201302), c(5, 15))
AddMonthsYM(c(201511, 201302), c(5, -4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
