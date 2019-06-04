\name{PlotMonth}
\alias{PlotMonth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Monthly or Seasonal Effects Of a Univariate Time Series
%%  ~~function to do ... ~~
}
\description{Plot monthly or seasonal effects of a univariate time series
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMonth(x, type = "l", labels, xlab = "", ylab = deparse(substitute(x)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{univariate time series
%%     ~~Describe \code{x} here~~
}
  \item{type}{todo
%%     ~~Describe \code{type} here~~
}
  \item{labels}{todo
%%     ~~Describe \code{labels} here~~
}
  \item{xlab}{a title for the x axis: see \code{\link{title}}.
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{a title for the y axis: see \code{\link{title}}.
%%     ~~Describe \code{ylab} here~~
}
  \item{\dots}{the dots are passed to the plot command.
%%     ~~Describe \code{\dots} here~~
}
}
\details{todo
%%  ~~ If necessary, more details than the description above ~~
}
\author{Markus Huerzeler, ETH Zurich
}

\seealso{\code{\link{ts}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotMonth(AirPassengers)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
