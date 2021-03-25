\name{PercentRank}
\alias{PercentRank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dense Ranks and Percent Ranks
%%  ~~function to do ... ~~
}
\description{
\code{DenseRank()} computes the rank of a vector \code{x} and returns the rank as a number. The ranks are consecutive integers beginning with 1. No ranks are skipped if there are ranks with multiple items. So the largest rank value is the number of unique values of x. Unlike \code{\link{rank}} gives the ranking within the vector x too, but ties are assigned the same rank, with the next ranking(s) skipped.

\code{PercentRank()} takes a vector \code{x} and returns the percentile that elements of \code{x} correspond to.
}
\usage{
PercentRank(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric, complex, character or logical vector.
%%     ~~Describe \code{x} here~~
}
}
\value{A numeric vector of the same length as \code{x} with names copied from \code{x} (unless \code{na.last = NA}, when missing values are removed). The vector is of integer type unless \code{x} is a long vector.
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

\seealso{\code{\link{Rank}}, \code{\link{rank}}, \code{\link{factor}}, \code{\link{order}}, \code{\link{sort}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))

x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2))        # ties are averaged

PercentRank(x2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
