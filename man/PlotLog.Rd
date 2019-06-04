\name{PlotLog}
\alias{PlotLog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logarithmic Plot
%%  ~~function to do ... ~~
}
\description{The base function \code{\link{grid}()} does not support logarithmic scales very well. Especially when more lines are required, grids have to be created manually. \code{PlotLog} creates a plot with at least one logarithmic axis and places a logarithmic grid in the background of the data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotLog(x, ..., args.grid = NULL, log = "xy")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the coordinates of points in the plot. Alternatively, a single plotting structure, function or any R object with a plot method can be provided.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{the dots are passed on to the function \code{plot()}.
%%     ~~Describe \code{\dots} here~~
}
  \item{args.grid}{a list of arguments for the grid. This contains line type, line width and line color, separately for major gridlines and for minor gridlines.\cr
  \code{args.grid=list(lwd=1, lty=3, col="grey85", col.min="grey60")} are used as default. If the argument is set to \code{NA}, no grid will be plotted.
%%     ~~Describe \code{grid} here~~
}
  \item{log}{a character string which contains "\code{x}" if the x axis is to be logarithmic, "\code{y}" if the y axis is to be logarithmic and "\code{xy}" or "\code{yx}" if both axes are to be logarithmic.
%%     ~~Describe \code{log} here~~
}
}

\value{nothing
%% the calculated positions of the x-ticks and y-ticks
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

\seealso{\code{\link{axis}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotLog(brain ~ body, data=MASS::Animals, log="xy",
        xlim=c(.01, 1e5), ylim=c(.1, 1e4), main="Animal brain/body size",
        pch=21, bg="grey", cex=1.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot}% __ONLY ONE__ keyword per line
