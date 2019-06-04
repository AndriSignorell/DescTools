\name{SplitAt}
\alias{SplitAt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a Vector Into Several Pieces at Given Positions
%%  ~~function to do ... ~~
}
\description{Split a vector into several pieces at given positions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SplitAt(x, pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be splitted.
%%     ~~Describe \code{x} here~~
}
  \item{pos}{integer vector, giving the positions at which the vector should be splitted.
%%     ~~Describe \code{pos} here~~
}
}
\value{a list with the splitted parts of x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{flodel (on StackOverflow)
%%  ~~who you are~~
}

\references{\url{https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position}}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{split}}, \code{\link{strsplit}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- 1:10
SplitAt(x, pos=c(3, 8))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }% __ONLY ONE__ keyword per line
\keyword{ category }% __ONLY ONE__ keyword per line
