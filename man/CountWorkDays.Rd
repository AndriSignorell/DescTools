\name{CountWorkDays}
\alias{CountWorkDays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Count Work Days Between Two Dates
%%  ~~function to do ... ~~
}
\description{Returns the number of work days between two dates taking into account the provided holiday dates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CountWorkDays(from, to, holiday = NULL, nonworkdays = c("Sat", "Sun"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{the initial dates
%%     ~~Describe \code{from} here~~
}
  \item{to}{the final dates
%%     ~~Describe \code{to} here~~
}
  \item{holiday}{a vector with dates to be excluded.
%%     ~~Describe \code{holiday} here~~
}
  \item{nonworkdays}{a character vector containing the abbreviations of the weekdays (as in \code{day.abb}) to be considered non work days. Default is \code{c("Sat","Sun")}.
%%     ~~Describe \code{nonCountWorkDays} here~~
}
}
\details{The function is vectorised so that multiple initial and final dates can be supplied. The dates are recycled, if their number are different
%%  ~~ If necessary, more details than the description above ~~
}
\value{an integer vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{weekdays}}, \code{Date Functions}

}
\examples{
from <- as.Date("2019-01-01") + rep(0, 10)
to <- as.Date("2020-01-15") + seq(0, 9)

CountWorkDays(from, to)

x <- seq(from[1], from[1]+11, "days")
data.frame(
  date = x, 
  day  = Format(x, fmt="ddd"))

CountWorkDays(from = min(x), to = max(x), holiday = c("2019-01-06", "2019-01-07"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ chron}
