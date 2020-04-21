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

\details{
Divisibility is a mathematical relationship between two integers. An integer is divisible by another integer if there is no remainder in the division. The number 11 has only two divisors: 1 and the number 11 itself, whereas the number 12 has many divisors: 1, 2, 3, 4, 6 and 12. 
In elementary number theory, the concept of divisibility is limited to natural numbers. 
The number of its divisors can be determined with the function \code{\link{length}()}.
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
