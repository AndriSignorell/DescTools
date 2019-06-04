\name{split.formula}
\alias{split.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula Interface for Split
%%  ~~function to do ... ~~
}
\description{Implementation of a simple formula interface for the \code{\link{split}} function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{split}{formula} (x, f, drop = FALSE, data = NULL, ...)

}
\arguments{
%- maybe also 'usage' for other objects documented here.
  \item{x}{a formula of the form \code{y ~ x}.}
\item{f}{a 'factor' in the sense that \code{\link{as.factor}(f)} defines the grouping, or a list of such factors in which case their interaction is used for the grouping.}
\item{drop}{logical indicating if levels that do not occur should be dropped (if \code{f} is a \code{factor} or a list).
Defaults to FALSE.}

  \item{data}{the data frame from which the formula should
  be evaluated.}


  \item{\dots}{other arguments to be passed to
  \code{\link{split}}.}
}
\author{Andri Signorell <andri@signorell>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{split}} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
split(extra ~ group, data = sleep)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ category }
