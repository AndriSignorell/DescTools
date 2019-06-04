\name{DigitSum}
\alias{DigitSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Digit Sum
%%  ~~function to do ... ~~
}
\description{Calculate digit sum of a number x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DigitSum(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an integer number
%%     ~~Describe \code{x} here~~
}
}
\value{the digit sum
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
URL: \url{http://stackoverflow.com/questions/18675285/digit-sum-function-in-r}
}
\author{Andri Signorell <andri@signorell.net> based on code by Julius benchmarked by Uwe
%%  ~~who you are~~
}


\seealso{\code{\link{IsPrime}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
DigitSum(c(124, 45, 268))
# [1]  7  9 16
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{arith}
