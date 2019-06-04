\name{Contrasts}
\alias{Contrasts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pairwise Contrasts
%%  ~~function to do ... ~~
}
\description{Generate all pairwise contrasts for using in a post-hoc test, e.g. ScheffeTest.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Contrasts(levs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{levs}{the levels to be used
%%     ~~Describe \code{levs} here~~
}
}
\value{A matrix with all possible pairwise contrasts, that can be built with the given levels.
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

\seealso{\code{\link{ScheffeTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Contrasts(LETTERS[1:5])

#   B-A C-A D-A E-A C-B D-B E-B D-C E-C E-D
# A  -1  -1  -1  -1   0   0   0   0   0   0
# B   1   0   0   0  -1  -1  -1   0   0   0
# C   0   1   0   0   1   0   0  -1  -1   0
# D   0   0   1   0   0   1   0   1   0  -1
# E   0   0   0   1   0   0   1   0   1   1

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
