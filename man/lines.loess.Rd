\name{lines.loess}
\alias{lines.loess}
\alias{lines.smooth.spline}
\alias{lines.SmoothSpline}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a Loess or a Spline Smoother
}
\description{
Add a loess smoother to an existing plot. The function first calculates the prediction of a loess object for a reasonable amount of points, then adds the line to the plot and inserts a polygon with the confidence intervals.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{lines}{loess}(x, col = Pal()[1], lwd = 2, lty = "solid",
      type = "l", n = 100, conf.level = 0.95, args.band = NULL, ...)

\method{lines}{smooth.spline}(x, col = Pal()[1], lwd = 2, lty = "solid",
      type = "l", conf.level = 0.95, args.band = NULL, ...)

\method{lines}{SmoothSpline}(x, col = Pal()[1], lwd = 2, lty = "solid",
      type = "l", conf.level = 0.95, args.band = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the loess or smooth.spline object to be plotted.
}
  \item{col}{linecolor of the smoother. Default is DescTools's \code{col1}.
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{line width of the smoother.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type of the smoother.
%%     ~~Describe \code{lwd} here~~
}
  \item{type}{type of plot, defaults to \code{"l"}.
%%     ~~Describe \code{type} here~~
}
  \item{n}{number of points used for plotting the fit.
%%     ~~Describe \code{type} here~~
}
  \item{conf.level}{confidence level for the confidence interval. Set this to NA, if no confidence band should be plotted.
  Default is 0.95.
%%     ~~Describe \code{type} here~~
}
  \item{args.band}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}).
%%     ~~Describe \code{type} here~~
}

  \item{\dots}{further arguments are passed to the smoother (\code{loess()} or \code{SmoothSpline()}).
%%     ~~Describe \code{\dots} here~~
}
}
\note{Loess can result in heavy computational load if there are many points!
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{loess}}, \code{\link{scatter.smooth}}, \code{\link{smooth.spline}}, \code{\link{SmoothSpline}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
par(mfrow=c(1,2))

x <- runif(100)
y <- rnorm(100)
plot(x, y)
lines(loess(y~x))

plot(temperature ~ delivery_min, data=d.pizza)
lines(loess(temperature ~ delivery_min, data=d.pizza))

plot(temperature ~ delivery_min, data=d.pizza)
lines(loess(temperature ~ delivery_min, data=d.pizza), conf.level = 0.99,
            args.band = list(col=SetAlpha("red", 0.4), border="black") )

# the default values from scatter.smooth
lines(loess(temperature ~ delivery_min, data=d.pizza,
            span=2/3, degree=1, family="symmetric"), col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\keyword{ aplot }

