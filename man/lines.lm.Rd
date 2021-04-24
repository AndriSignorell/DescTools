\name{lines.lm}
\alias{lines.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add a Linear Regression Line
%%  ~~function to do ... ~~
}
\description{
Add a linear regression line to an existing plot. The function first calculates the prediction of a \code{lm} object for a reasonable amount of points, then adds the line to the plot and inserts a polygon with the confidence and, if required, the prediction intervals.
In addition to \code{\link{abline}} the function will also display polynomial models.
}
\usage{
\method{lines}{lm}(x, col = Pal()[1], lwd = 2, lty = "solid",
      type = "l", n = 100, conf.level = 0.95, args.cband = NULL,
      pred.level = NA, args.pband = NULL, xpred = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{linear model object as result from lm(y~x).
}
  \item{col}{linecolor of the line. Default is the color returned by \code{Pal()[1]}.
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{line width of the line.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type of the line.
%%     ~~Describe \code{lwd} here~~
}
  \item{type}{character indicating the type of plotting; actually any of the \code{types} as in \code{\link{plot.default}}. Type of plot, defaults to \code{"l"}.
%%     ~~Describe \code{type} here~~
}
  \item{n}{number of points used for plotting the fit.
%%     ~~Describe \code{type} here~~
}
  \item{conf.level}{confidence level for the confidence interval. Set this to \code{NA}, if no confidence band should be plotted.
  Default is \code{0.95}.
%%     ~~Describe \code{type} here~~
}
  \item{args.cband}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}).
%%     ~~Describe \code{type} here~~
}
  \item{pred.level}{confidence level for the prediction interval. Set this to NA, if no prediction band should be plotted.
  Default is \code{0.95}.
%%     ~~Describe \code{type} here~~
}
  \item{args.pband}{list of arguments for the prediction band, such as color or border (see \code{\link{DrawBand}}).
%%     ~~Describe \code{type} here~~
}
\item{xpred}{a numeric vector \code{c(from, to)},  if the x limits can't be defined based on available data, xpred can be used to provide the range where the line and especially the confidence intervals should be plotted.}
  \item{\dots}{further arguments are not used specifically.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
It's sometimes illuminating to plot a regression line with its prediction, resp. confidence intervals over an existing scatterplot. This only makes sense, if just a simple linear model explaining a target variable by (a function of) one single predictor is to be visualized.
%%  ~~ If necessary, more details than the description above ~~
}
\value{nothing
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{lines}}, \code{\link{lines.loess}}, \code{\link{lm}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
opar <- par(mfrow=c(1,2))

plot(hp ~ wt, mtcars)
lines(lm(hp ~ wt, mtcars), col="steelblue")

# add the prediction intervals in different color
plot(hp ~ wt, mtcars)
r.lm <- lm(hp ~ wt, mtcars)
lines(r.lm, col="red", pred.level=0.95, args.pband=list(col=SetAlpha("grey",0.3)) )

# works with transformations too
plot(dist ~ sqrt(speed), cars)
lines(lm(dist ~ sqrt(speed), cars), col=hred)

plot(dist ~ log(speed), cars)
lines(lm(dist ~ log(speed), cars), col=hred)

# and with more specific variables based on only one predictor
plot(dist ~ speed, cars)
lines(lm(dist ~ poly(speed, degree=2), cars), col=hred)

par(opar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{math}
