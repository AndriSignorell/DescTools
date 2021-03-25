\name{StrPos}
\alias{StrPos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find Position of First Occurrence Of a String
%%  ~~function to do ... ~~
}
\description{Returns the numeric position of the first occurrence of a substring within a string. If the search string is not found, the result will be \code{NA}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrPos(x, pattern, pos = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector in which to search for the pattern, or an object which can be coerced by as.character to a character vector.
%%     ~~Describe \code{text} here~~
}
  \item{pattern}{character string (search string) containing the pattern to be matched in the given character vector. This can be a character string or a regular expression.
%%     ~~Describe \code{pattern} here~~
}
  \item{pos}{integer, defining the start position for the search within x. The result will then be relative to the begin of the truncated string. Will be recycled.
%%     ~~Describe \code{text} here~~
}
  \item{\dots}{the dots are passed to the function \code{\link{regexpr}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{This is just a wrapper for the function \code{\link{regexpr}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector of the first position of pattern in  x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{StrChop}}, \code{\link{regexpr}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StrPos(x = levels(d.pizza$driver), pattern = "t")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
\concept{ string }
