\name{ExpFreq}
\alias{ExpFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expected Frequencies
%%  ~~function to do ... ~~
}
\description{Calculate the expected frequencies of an n-way table assuming independence.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ExpFreq(x, freq = c("abs", "rel"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a table.
%%     ~~Describe \code{x} here~~
}
  \item{freq}{indicates, whether absolute or relative frenquencies should be computed. Can either
  be \code{"abs"} or \code{"rel"}. Partial matching is supported.
%%     ~~Describe \code{freq} here~~
}
}
\value{A table with either the absolute or the relative expected frequencies.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{This is a copy of the function \code{independence_table} in \pkg{vcd}.
%%  ~~further notes~~
}
\author{David Meyer <David.Meyer@R-project.org>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{chisq.test}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ExpFreq(Titanic)

ExpFreq(UCBAdmissions, freq="r")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
