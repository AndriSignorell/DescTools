\name{IsDate}
\alias{IsDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check If an Object Is of Type Date
%%  ~~function to do ... ~~
}
\description{Check if the given x is of any known Date type.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsDate(x, what = c("either", "both", "timeVaries"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector or values to be checked.
%%     ~~Describe \code{x} here~~
}
  \item{what}{can be any value out of "\code{either}" (default), "\code{both}" or "\code{timeVaries}".
%%     ~~Describe \code{what} here~~
}
}
\details{This checks for many known Date and Time classes: "POSIXt", "POSIXct", "dates", "times", "chron", "Date".
%%  ~~ If necessary, more details than the description above ~~
}
\value{logical vector of the same dimension as x.
}
\author{Frank E Harrell
%%  ~~who you EE. are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
IsDate(as.Date("2013-04-10"))

IsDate(31002)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
\keyword{ logic }

