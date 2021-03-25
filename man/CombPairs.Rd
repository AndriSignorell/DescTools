\name{CombPairs}
\alias{CombPairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get All Pairs Out of One or Two Sets of Elements
%%  ~~function to do ... ~~
}
\description{Returns all combinations of 2 out of the elements in x or x and y (if defined). Combinations of the same elements will be dropped (no replacing).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CombPairs(x, y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of elements
%%     ~~Describe \code{x} here~~
}
  \item{y}{a vector of elements, need not be same dimension as x.
  If y is not \code{NULL} then all combination x and y are returned.
%%     ~~Describe \code{y} here~~
}
}
\details{If y = \code{NULL} then all combination of 2 out of x are returned. \cr
If y is defined then all combinations of x and y are calculated.
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{CombPairs} returns a data.frame with 2 columns X1 and X2.
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{combn}}, \code{\link{expand.grid}}, \code{\link{outer}}, \code{\link{lower.tri}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
CombPairs(letters[1:4])
CombPairs(x = letters[1:4], y = LETTERS[1:2])

# get all pairs of combinations between factors and numerics out of a data.frame
CombPairs(which(sapply(d.pizza, IsNumeric)), which(sapply(d.pizza, is.factor)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\concept{ combinatorics }

