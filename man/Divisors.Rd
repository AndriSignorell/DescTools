\name{Divisors}
\alias{Divisors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Divisors
%%  ~~function to do ... ~~
}
\description{Calculate divisors of positive natural numbers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Divisors(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{integer number for which the divisors are to be returned
%%     ~~Describe \code{x} here~~
}
}
\value{an integer vector containg the divisors
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{Primes}}, \code{\link{IsPrime}}, \code{\link{GCD}}, \code{\link{LCM}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Divisors(c(145, 786))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }% __ONLY ONE__ keyword per line
\keyword{ math }% __ONLY ONE__ keyword per line
