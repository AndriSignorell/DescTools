\name{StrVal}
\alias{StrVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract All Numeric Values From a String
%%  ~~function to do ... ~~
}
\description{Extract all numeric values from a string using a regular expression and return a list of all found values. If there are several, the values can be either pasted and/or casted from characters to numeric values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrVal(x, paste = FALSE, as.numeric = FALSE, dec = getOption("OutDec"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector
%%     ~~Describe \code{x} here~~
}
\item{paste}{should separatetly extracted numbers be pasted together? This can be useful to reverse a prior format action. Default is \code{FALSE}.}
\item{as.numeric}{logical value, determining if the extracted values should be converted to a number or be returned as characters. Default is \code{FALSE}.}
\item{dec}{character string containing a single character. The preferred character to be used as the decimal point. Defaults \code{getOption("OutDec")}. }
}
\details{If there are multiple numbers in the same string to paste and cast to numeric, pasting will be done first and after pasting the conversion will be performed. So if for example the numbers in \code{x = "34 way 066"} should be extracted \code{StrVal(x, paste = TRUE, as.numeric = TRUE)} will lead to \code{34066}. This is a useful choice for converting formatted numbers having some kind of bigmark. }

\value{depending on the results the function will return either a character vector, in the case every element of x contained only one number, or a list of character vectors containing the found numbers.
}
\author{Andri Signorell <andri@signorell.net>, Markus Naepflin <markus@naepfl.in> provided an optimized regex
}

\seealso{other string functions in \code{\link{DescTools-package}}, section \code{String functions}
}
\examples{
# a simple vector with only one number per element
StrVal(x=c("week 1", "week 3", "week 4", "week 5"))

# several numbers per element, extract each part, do not paste and return characters
StrVal(x=c("This is 1. place: 45.2", "none", "12.1 but -2.7 follow, 10.2e23 "),
       paste = FALSE, as.numeric = FALSE)

# critical are numbers combined with signs, where we sequentially extract valid numbers
StrVal(x=c("78-23-99", "1e-15-34*789+9", "- 34values"),
       paste = FALSE, as.numeric = FALSE)

# a typical use case for this function is to reverse a previously
#   applied number format

x <- c(100000, 4564654632, -456463)
xf <- Format(x, big.mark="'")

StrVal(xf, paste = TRUE, as.numeric = TRUE)

StrVal(xf, paste = TRUE, as.numeric = FALSE)
StrVal(xf, paste = FALSE, as.numeric = TRUE)
StrVal(xf, paste = FALSE, as.numeric = FALSE)

# use an alternative decimal point
StrVal("8 452,12", dec=",")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
