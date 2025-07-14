\name{as.CDateFmt}
\alias{as.CDateFmt}

\title{Format Codes for Dates
}
\description{Converts descriptive ISO-8601 codes into the cryptic C-format 
codes used in base R.
}
\usage{
as.CDateFmt(fmt)
}
\arguments{
  \item{fmt}{a character, any form of ISO-8601-date-and-time-format codes (e.g. "dd-mm-yyyy"). 
  See Details.
}
}
\details{
The argument \code{fmt} takes ISO-8601-date-and-time-format codes consisting of 
(\code{d}, \code{m} and \code{y} for day, month or year) and 
defining the combination of day month and year representation. 
Repeating the specific code defines the degree of abbreviation. \cr
So the format \code{'yyyy-mm-dd'} would yield a date as \code{2020-10-12}.

\tabular{ll}{
\bold{Date Codes} \tab  \cr
  \code{d } \tab  day of the month without leading zero (1 - 31) \cr
  \code{dd} \tab  day of the month with leading zero (01 - 31)\cr
  \code{ddd} \tab abbreviated name for the day of the week (e.g. Mon) in the current user's language  \cr
                  \code{dddd} \tab    full name for the day of the week (e.g. Monday) in the current user's language  \cr
                  \code{m } \tab  month without leading zero (1 - 12) \cr
   \code{mm} \tab    month with leading zero (01 - 12) \cr
                 \code{mmm }  \tab   abbreviated month name (e.g. Jan) in the current user's language \cr
                \code{mmmm}  \tab    full month name (e.g. January) in the current user's language \cr
            \code{y } \tab   year without century, without leading zero (0 - 99) \cr
                 \code{yy } \tab      year without century, with leading zero (00 - 99) \cr
            \code{yyyy }  \tab   year with century. For example: 2005 \cr
\cr
}
This function is used in \code{\link{Fm}()} to format dates.

}
\value{the formatted dates as characters.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{Fm}}
}
\examples{
as.CDateFmt(fmt="ddd, d mmmm yyyy")

# Fm uses the function internally, the codes can be provided directly as:
Fm(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy")
Fm(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy", lang="engl")

# for using your local language:
Fm(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy", lang="local")
}
\keyword{ IO }
