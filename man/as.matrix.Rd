\name{as.matrix.xtabs}
\alias{as.matrix.xtabs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert xtabs To matrix
%%  ~~function to do ... ~~
}
\description{This function converts an \code{xtabs} object to a \code{matrix}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{as.matrix}{xtabs}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{xtabs}
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{additional arguments to be passed to or from methods.
%%     ~~Describe \code{\dots} here~~
}
}
\details{An \code{\link{xtabs}} object is indeed already a matrix, but won't be converted to a pure matrix by \code{as.matrix.default} function, as its class definition will remain unchanged. Some functions expecting a pure matrix may fail, when fed with a \code{xtabs} object.
\code{as.matrix.xtabs} will drop the classes and the call attribute. \cr
Note that \code{\link{unclass}} would as well discard the classes \code{xtabs} and \code{table}, but retain the \code{"call"} attribute.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{as.matrix}}, \code{\link{xtabs}}, \code{\link{unclass}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tab <- xtabs( ~ driver + operator, data=d.pizza)

str(tab)
class(tab)

str(as.matrix(tab))
class(as.matrix(tab))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manipulate }% __ONLY ONE__ keyword per line
