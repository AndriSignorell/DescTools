\name{StrLeft, StrRight}
\alias{StrRight}
\alias{StrLeft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Returns the Left Or the Right Part Of a String
%%  ~~function to do ... ~~
}
\description{Returns the left part or the right part of a string. The number of characters are defined by the argument \code{n}.
If \code{n} is negative, this number of characters will be cut off from the other side.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrLeft(x, n)
StrRight(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of strings.
%%     ~~Describe \code{x} here~~
}
  \item{n}{a positive or a negative integer, the number of characters to cut. If n is negative, this number of characters will be cut off
  from the right with \code{StrLeft} and from the right with \code{StrRight}. \cr n will be recycled.
%%     ~~Describe \code{n} here~~
}
}
\details{The functions \code{StrLeft} and  \code{StrRight} are simple wrappers to \code{substr}.}
\value{the left (right) n characters of x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{substr}}, \code{\link{StrTrim}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StrLeft("Hello world!", n=5)
StrLeft("Hello world!", n=-5)

StrRight("Hello world!", n=6)
StrRight("Hello world!", n=-6)

StrLeft(c("Lorem", "ipsum", "dolor","sit","amet"), n=2)

StrRight(c("Lorem", "ipsum", "dolor","sit","amet"), n=c(2,3))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ character }% __ONLY ONE__ keyword per line
