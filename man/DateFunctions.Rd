\name{Date Functions}
\alias{Year}
\alias{Quarter}
\alias{Month}
\alias{Week}
\alias{Day}
\alias{Day<-}
\alias{Weekday}
\alias{YearDay}
\alias{YearMonth}
\alias{IsWeekend}
\alias{IsLeapYear}
\alias{Hour}
\alias{Minute}
\alias{Second}
\alias{Now}
\alias{Today}
\alias{DiffDays360}
\alias{LastDayOfMonth}
\alias{Timezone}
\alias{YearDays}
\alias{MonthDays}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basic Date Functions
%%  ~~function to do ... ~~
}
\description{Some more date functions for making daily life a bit easier. The first ones extract a specific part of a given date, others check some conditions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Year(x)
Quarter(x)
Month(x, fmt = c("m", "mm", "mmm"), lang = DescToolsOptions("lang"),
      stringsAsFactors = TRUE)
Week(x, method = c("iso", "us"))
Day(x)
Weekday(x, fmt = c("d", "dd", "ddd"), lang = DescToolsOptions("lang"),
        stringsAsFactors = TRUE)
YearDay(x)
YearMonth(x)

Day(x) <- value

IsWeekend(x)
IsLeapYear(x)

Hour(x)
Minute(x)
Second(x)
Timezone(x)

Now()
Today()

DiffDays360(start_d, end_d, method = c("eu", "us"))
LastDayOfMonth(x)
YearDays(x)
MonthDays(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the date to be evaluated.
%%     ~~Describe \code{x} here~~
}
  \item{fmt}{format string, defines how the month or the weekday are to be formatted. Defaults to \code{"m"}, resp. \code{"d"}. Is ignored for other functions.
%%     ~~Describe \code{format} here~~
}
  \item{value}{new value
%%     ~~Describe \code{value} here~~
}
  \item{lang}{optional value setting the language for the months and daynames. Can be either \code{"local"} for current locale or \code{"engl"} for english. If left to \code{NULL}, the option \code{"lang"} will be searched for and if not found \code{"local"} will be taken as default.
%%     ~~Describe \code{value} here~~
}
  \item{stringsAsFactors}{logical. Defines if the result should be coerced to a factor, using the local definitions as levels.
  The result would be an ordered factor. Default is TRUE.
}
  \item{start_d, end_d}{the start, resp. end date for \code{DiffDays360}.
%%     ~~Describe \code{value} here~~
}
  \item{method}{one out of \code{"eu", "us"}, setting either European or US-Method calculation mode. Default is \code{"eu"}.
%%     ~~Describe \code{value} here~~
}

}

\details{These functions are mainly convenience wrappers for the painful \code{format()} and its strange codes...\cr
Based on the requested time component, the output is as follows:\cr\cr
\code{Year} returns the year of the input date in yyyy format.\cr
\code{Quarter} returns the quarter of the year (1 to 4) for the input date. \cr
\code{Month} returns the month of the year (1 to 12) for the input date. \cr
\code{Week} returns the week of the year for the input date (0 to 53), as defined in ISO8601. \cr
\code{Weekday} returns the week day of the input date. (1 - Monday, 2 - Tuesday, ... 7 - Sunday). (Names and abbreviations are either english or in the current locale!)\cr
\code{YearDay} returns the day of the year numbering (1 to 366). \cr
\code{Day} returns the day of the month (1 to 31). \cr
\code{YearMonth} returns the yearmonth representation (yyyymm) of a date as long integer. \cr
\code{Hour}, \code{Minute}, \code{Second}, \code{Timezone} return the hour, minute, second or timezone from a POSIXlt object. \cr
\code{Today}, \code{Now} return the current date, resp. the current date and time.\cr

\code{IsWeekend} returns \code{TRUE}, if the date x falls on a weekend. \cr
\code{IsLeapYear} returns \code{TRUE}, if the year of the date x is a leap year. \cr

The day can not only be extracted, but as well be defined. See examples.

\code{DiffDays360} calculates the difference between 2 dates using the 360-days convention.\cr
\code{LastDayOfMonth} returns the last day of the month of the given date(s).
\code{YearDays} returns the total number of days of the given date(s).
\code{MonthDays} returns the numer of days of the month of the given date(s).
%%  ~~ If necessary, more details than the description above ~~

The language in Weekday and Moth can be set with an option as well. The functions will check for an existing option named "lang" and take this value if it exists. So simply set option(lang="engl") if the results should always be reported in English.

}
\value{a vector of the same dimension as x, consisting of either numeric values or characters depending on the function used.
}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{strptime}}, \code{\link{DateTimeClasses}}, \code{\link{as.POSIXlt}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- Today()    # the same as Sys.Date() but maybe easier to remember..

Year(x)
Quarter(x)

Month(x)
Month(x, fmt = "mm", lang="engl")
Month(x, fmt = "mm", lang="local")
Month(x, fmt = "mmm", lang="engl")
Month(x, fmt = "mmm", lang="local")

Week(x)

Day(x)
Day(x) <- 20
x

Weekday(x)
Weekday(x, fmt = "dd", lang="engl")
Weekday(x, fmt = "dd", lang="local")
Weekday(x, fmt = "ddd", lang="engl")
Weekday(x, fmt = "ddd", lang="local")

YearDay(x)

IsWeekend(x)

IsLeapYear(x)

# let's generate a time sequence by weeks
Month(seq(from=as.Date(Sys.Date()), to=Sys.Date()+150, by="weeks"), fmt="mm")

LastDayOfMonth(as.Date(c("2014-10-12","2013-01-31","2011-12-05")))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{chron}
\concept{ dates }

