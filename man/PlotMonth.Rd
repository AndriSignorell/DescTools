\name{PlotMonth}
\alias{PlotMonth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cycle Plot for Seasonal Effects of an Univariate Time Series
%%  ~~function to do ... ~~
}
\description{Plot seasonal effects of a univariate time series following Cleveland's definition for cycle plots.
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
  \item{type}{one out of \code{"l"} (line) or \code{"h"} (histogram), defines the plot type of the year components
%%     ~~Describe \code{type} here~~
}
  \item{labels}{the labels for the cyclic component to be displayed on the x-axis
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
\details{A cycle plot is a graphical method invented to study teh behaviour of a seasonal time series. The seasonal component of a univariate series is graphed. First the January values are graphed for successive years, then the February values and so forth. For each monthly subseries the mean of the values is portrayed by a horizontal line.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Markus Huerzeler (ETH Zurich), slight changes Andri Signorell <andri@signorell.net>
}
\references{  Cleveland, W. S. (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, CA: Wadsworth.}
  
\seealso{\code{\link{ts}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotMonth(AirPassengers)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
