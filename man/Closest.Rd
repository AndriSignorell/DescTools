\name{Closest}
\alias{Closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the Closest Value 
%%  ~~function to do ... ~~
}
\description{Find the closest value(s) of a number in a vector x. Multiple values will be reported, if the differences are the same.}
\usage{
Closest(x, a, which = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be searched in
%%     ~~Describe \code{x} here~~
}
  \item{a}{the reference value
%%     ~~Describe \code{a} here~~
}
\item{which}{a logical value defining if the index position or the value should be returned. By default will the value be returned.}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
%%     ~~Describe \code{na.rm} here~~
}
}
\value{the value or index in x which is closest to a
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

\seealso{ \code{\link{which}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(8)
x <- sample(10, size=10, replace=TRUE)

Closest(x, 6)
Closest(x, 6, which=TRUE)


Closest(c(2, 3, 4, 5), 3.5)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ logic }% __ONLY ONE__ keyword per line
