\name{Shade}
\alias{Shade}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce a Shaded Curve
%%  ~~function to do ... ~~
}
\description{
Sometimes the area under a density curve has to be color shaded, for instance to illustrate a p-value or a specific region under the normal curve.
This function draws a curve corresponding to a function over the interval \code{[from, to]}. It can plot also an expression in the variable \code{xname}, default \code{x}.
}
\usage{
Shade(expr, col = par("fg"), breaks, density = 10, n = 101, xname = "x", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{the name of a function, or a \code{\link{call}} or an \code{\link{expression}} written as a function of \code{x} which will evaluate to an object of the same length as \code{x}.
%%     ~~Describe \code{FUN} here~~
}
  \item{col}{color to fill or shade the shape with. The default is taken from \code{par("fg")}.
%%     ~~Describe \code{col} here~~
}
  \item{breaks}{numeric, a vector giving the breakpoints between the distinct areas to be shaded differently. Should be finite as there are no plots with infinite limits.
%%     ~~Describe \code{xlim} here~~
}
  \item{density}{the density of the lines as needed in polygon.
%%     ~~Describe \code{density} here~~
}
  \item{n}{integer; the number of x values at which to evaluate. Default is 101.
%%     ~~Describe \code{step} here~~
}
\item{xname}{character string giving the name to be used for the x axis.}

  \item{\dots}{the dots are passed on to \code{\link{polygon}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Useful for shading the area under a curve as often needed for explaining significance tests.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components \code{x} and \code{y} of the points that were drawn is returned invisibly.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{polygon}}, \code{\link{curve}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
curve(dt(x, df=5), xlim=c(-6,6),
      main=paste("Student t-Distribution Probability Density Function, df = ", 5, ")", sep=""),
      type="n", las=1, ylab="probability", xlab="t")

Shade(dt(x, df=5), breaks=c(-6, qt(0.025, df=5), qt(0.975, df=5), 6),
      col=c(hred, hblue), density=c(20, 7))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot }% use one of  RShowDoc("KEYWORDS")
