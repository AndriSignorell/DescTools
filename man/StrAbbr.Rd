\name{StrAbbr}
\alias{StrAbbr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{String Abbreviation
%%  ~~function to do ... ~~
}
\description{Abbreviate a character vector. The function includes starting from the first character 
as many characters as there are needed to result in a vector of unique values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrAbbr(x, minchar = 1, method = c("left", "fix"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character vector to be abbreviated
%%     ~~Describe \code{x} here~~
}
  \item{minchar}{integer, minimal number of characters for the abbreviations.
%%     ~~Describe \code{minchar} here~~
}
  \item{method}{one out of \code{left} or \code{fix}. While \code{left} restricts the result to as many characters 
  are needed to ensure uniqueness, does \code{fix} yield a vector with all the elements being as long, as 
  the the longest needed substring for differentiating the terms.
%%     ~~Describe \code{method} here~~
}
}
\value{The abbreviated strings.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{abbreviate}}, \code{\link{StrTrunc}}, \code{\link{StrTrim}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StrAbbr(x=levels(d.pizza$driver), minchar=2)
StrAbbr(x=levels(d.pizza$driver), minchar=2, method="left")
StrAbbr(x=levels(d.pizza$driver), minchar=2, method="fix")


x <- c("Aaron", "Aaramis", "Berta", "Bello", "Claudia", "Cardinale", "Doretta", "Emilia")
StrAbbr(x, minchar=2, method="left")
StrAbbr(x, minchar=2, method="fix")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
