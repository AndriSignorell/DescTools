\name{Sample}
\alias{Sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Samples and Permutations
%%  ~~function to do ... ~~
}
\description{\code{Sample} takes a sample of the specified size from the elements of x using either with or without replacement. The function does the same as the base::sample() and offers additionally an interface for data frames.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sample(x, size, replace = FALSE, prob = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a vector of one or more elements from which to choose, or a positive integer.
}
  \item{size}{a positive number, the number of items to choose from.
%%     ~~Describe \code{size} here~~
}
  \item{replace}{a non-negative integer giving the number of items to choose.
%%     ~~Describe \code{replace} here~~
}
  \item{prob}{should sampling be with replacement?
%%     ~~Describe \code{prob} here~~
}
}
\value{sampled elements in the same structure as x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{sample}}
}
\examples{
sample(d.pizza, size=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% __ONLY ONE__ keyword per line
\keyword{ distribution }% __ONLY ONE__ keyword per line
