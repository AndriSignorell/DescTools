\name{ErrBars}
\alias{ErrBars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Error Bars to an Existing Plot
%%  ~~function to do ... ~~
}
\description{Add error bars to an existing plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ErrBars(from, to = NULL, pos = NULL, mid = NULL, horiz = FALSE, col = par("fg"),
        lty = par("lty"), lwd = par("lwd"), code = 3, length = 0.05,
        pch = NA, cex.pch = par("cex"), col.pch = par("fg"), bg.pch = par("bg"),
        \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{coordinates of points \bold{from} which to draw (the lower end of the error bars). If \code{to} is left to \code{NULL} and \code{from} is a \eqn{k \times 2 }{k x 2}  dimensional matrix, the first column will be interpreted as \code{from} and the second as \code{to}.
%%     ~~Describe \code{x0} here~~
}
  \item{to}{coordinates of points \bold{to} which to draw (the upper end of the error bars).
%%     ~~Describe \code{x1} here~~
}
  \item{pos}{numeric, position of the error bars. This will either be the x-coordinate in case of vertical error bars
  and the y-coordinate in case of horizontal error bars.
%%     ~~Describe \code{y} here~~
}
  \item{mid}{numeric, position of midpoints. Defaults to the mean of \code{from} and \code{to}.
%%     ~~Describe \code{col} here~~
}
  \item{horiz}{logical, determining whether horizontal error bars are needed (default is FALSE).
%%     ~~Describe \code{col} here~~
}
  \item{col}{the line color.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{the line type.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width.
%%     ~~Describe \code{lwd} here~~
}
  \item{code}{integer code, determining where end lines are to be drawn. \code{code = 0} will draw no end lines, \code{code = 1} will draw an end line on the left (lower) side at (\code{x0[i]}, \code{y0[i]}), \code{code = 2} on the right (upper) side (\code{x1[i]}, \code{y1[i]}) and
  \code{code = 3} (default) will draw end lines at both ends.
%%     ~~Describe \code{code} here~~
}
  \item{length}{the length of the end lines.
%%     ~~Describe \code{length} here~~
}
  \item{pch}{plotting character for the midpoints. The position of the points is given by \code{mid}. If \code{mid} is left to \code{NULL} the points will be plotted in the middle of \code{from} and \code{to}. No points will be plotted if this is set to \code{NA}, which is the default.
}
  \item{cex.pch}{the character extension for the plotting characters. Default is \code{par("cex")}.}
  \item{col.pch}{the color of the plotting characters. Default is \code{par("fg")}.
  }
  \item{bg.pch}{the background color of the plotting characters (if pch is set to 21:25). Default is \code{par("bg")}.}

  \item{\dots}{the dots are passed to the \code{\link{arrows}} function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{A short wrapper for plotting error bars by means of \code{\link{arrows}}.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{arrows}}, \code{\link{lines.loess}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

par(mfrow=c(2,2))
b <- barplot(1:5, ylim=c(0,6))
ErrBars(from=1:5-rep(0.5,5), to=1:5+rep(0.5,5), pos=b, length=0.2)

# just on one side
b <- barplot(1:5, ylim=c(0,6))
ErrBars(from=1:5, to=1:5+rep(0.5,5), pos=b, length=0.2, col="red", code=2, lwd=2)

b <- barplot(1:5, xlim=c(0,6), horiz=TRUE)
ErrBars(from=1:5, to=1:5+rep(0.2,5), pos=b, horiz=TRUE,  length=0.2, col="red", code=2, lwd=2)

par(xpd=FALSE)
dotchart(1:5, xlim=c(0,6))
ErrBars(from=1:5-rep(0.2,5), to=1:5+rep(0.2,5), horiz=TRUE, length=0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
