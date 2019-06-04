\name{StrIsNumeric}
\alias{StrIsNumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Does a String Contain Only Numeric Data
%%  ~~function to do ... ~~
}
\description{Check whether a string does only contain numeric data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrIsNumeric(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector
%%     ~~Describe \code{x} here~~
}
}
\value{a logical vector with the same dimension as x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{Other string functions, e.g. \code{\link{StrTrunc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c("123", "-3.141", "foobar123")
StrIsNumeric(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character}
