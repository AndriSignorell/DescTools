\name{Zodiac}
\alias{Zodiac}
\title{Calculate the Zodiac of a Date
}
\description{Calculate the sign of zodiac of a date.
}
\usage{
Zodiac(x, lang = c("engl", "deu"), stringsAsFactors = TRUE)
}
\arguments{
  \item{x}{the date to be transformed.}
  \item{lang}{the language of the zodiac names, can be english (default) or german (\code{"deu"}).}
  \item{stringsAsFactors}{logical. If set to \code{TRUE} (default) the result will consist of a factor with zodiac signs as levels.
}
}
\details{The really relevant things can sometimes hardly be found. You just discovered such a function... ;-)

The following rule to determine zodiac symbols is implemented:

\preformatted{Dec. 22  - Jan. 19  : Capricorn 
Jan. 20  - Feb. 17  : Aquarius 
Feb. 18  - Mar. 19  : Pisces 
March 20 - April 19 : Aries 
April 20 - May 19   : Taurus 
May 20   - June 20  : Gemini 
June 21  - July 21  : Cancer 
July 22  - Aug. 22  : Leo 
Aug 23   - Sept. 21 : Virgo 
Sept. 22 - Oct. 22  : Libran 
Oct. 23  - Nov. 21  : Scorpio 
Nov. 22  - Dec. 21  : Sagittarius
}
}
\value{character vector or factor with the zodiac.
}
\author{Andri Signorell <andri@signorell.net>, based on code from Markus Naepflin
}
\seealso{\code{\link{Year}} and other date functions
}
\examples{
Zodiac(as.Date(c("1937-07-28", "1936-06-01", "1966-02-25",
                 "1964-11-17", "1972-04-25")), lang="deu")

d <- sample(seq(as.Date("2015-01-01"), as.Date("2015-12-31"), 1), 120)
z <- Zodiac(d)
Desc(z)
}
\keyword{ chron }
