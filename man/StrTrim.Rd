\name{StrTrim}
\alias{StrTrim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Leading/Trailing Whitespace From A String
%%  ~~function to do ... ~~
}
\description{
The function removes whitespace characters as spaces, tabs and newlines from the beginning and end of the supplied string.
Whitespace characters occurring in the middle of the string are retained.\cr
Trimming with method \code{"left"} deletes only leading whitespaces, \code{"right"} only trailing.
Designed for users who were socialized by SQL.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrTrim(x, pattern = " \\t\\n", method = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  the string to be trimmed.
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{
  the pattern of the whitespaces to be deleted, defaults to space, tab and newline: \code{" \\t\\n"}.
%%     ~~Describe \code{x} here~~
}
  \item{method}{one out of \code{"both"} (default), \code{"left"}, \code{"right"}. Determines on which side the string should be trimmed.}
}
\details{
The functions are defined depending on method as\cr
  \code{both:  gsub( pattern=gettextf("^[\%s]+|[\%s]+$", pattern, pattern), replacement="", x=x)}\cr
  \code{left:  gsub( pattern=gettextf("^[\%s]+",pattern), replacement="", x=x)}\cr
  \code{right: gsub( pattern=gettextf("[\%s]+$",pattern), replacement="", x=x)}
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the string x without whitespaces}


\author{Andri Signorell <andri@signorell.net>
}


\seealso{ String functions:
\code{trimws}, \code{\link{nchar}}, \code{\link{match}}, \code{\link{grep}}, \code{\link{regexpr}}, \code{\link{substr}}, \code{\link{sub}}, \code{\link{gsub}},
\code{\link{StrTrunc}}, \code{\link{StrDist}}

}


\examples{
StrTrim("  Hello world! ")

StrTrim("  Hello world! ", method="left")
StrTrim("  Hello world! ", method="right")

# user defined pattern
StrTrim(" ..Hello ... world! ", pattern=" \\\\.")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ character }% __ONLY ONE__ keyword per line
\concept{ string }

