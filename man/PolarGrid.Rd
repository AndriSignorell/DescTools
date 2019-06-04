\name{PolarGrid}
\alias{PolarGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Grid in Polar Coordinates
%%  ~~function to do ... ~~
}
\description{\code{PolarGrid} adds a polar grid to an existing plot. The number of radial gridlines are
set by \code{ntheta} and the tangential lines by \code{nr}. Labels for the angular grid and the radial axis can be provided.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PolarGrid(nr = NULL, ntheta = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"),
          rlabels = NULL, alabels = NULL, lblradians = FALSE, cex.lab = 1, las = 1,
          adj = NULL, dist = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nr}{number of circles. When NULL, as per default, the grid aligns with the tick marks on the corresponding default axis
   (i.e., tickmarks as computed by axTicks). When NA, no circular grid lines are drawn.

%%     ~~Describe \code{nr} here~~
}
  \item{ntheta}{number of radial grid lines. Defaults to 12 uniformly distributed between 0 and 2*pi (each pi/3).
%%     ~~Describe \code{ntheta} here~~
}
  \item{col}{character or (integer) numeric; color of the grid lines.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{character or (integer) numeric; line type of the grid lines.

%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{non-negative numeric giving line width of the grid lines.

%%     ~~Describe \code{lwd} here~~
}
   \item{rlabels}{the radius labels. Use \code{NA} if no labels should be to be added.
%%     ~~Describe \code{rlabels} here~~
}
  \item{alabels}{the labels for the angles, they are printed on a circle outside the plot. Use \code{NA} for no angle labels.
%%     ~~Describe \code{anglabels} here~~
}
  \item{lblradians}{logic, defines if angle labels will be in degrees (default) or in radians.
%%     ~~Describe \code{lblradians} here~~
}
\item{cex.lab}{the character extension for the labels.
%%     ~~Describe \code{cex.lab} here~~
}
\item{las}{alignment of the labels, 1 means horizontal, 2 radial and 3 vertical.
%%     ~~Describe \code{labels} here~~
}
 \item{adj}{adjustments for the labels. (Left: 0, Right: 1, Mid: 0.5) The default is 1 for the levels on the right side of the circle, 0 for labels on the left and 0.5 for labels exactly on north on south.
%%     ~~Describe \code{labels} here~~
}
\item{dist}{gives the radius for the labels, in user coordinates. Default is par("usr")[2] * 1.07.}

}
%%\details{This can be made better....
%%  ~~ If necessary, more details than the description above ~~
%%}

\author{Andri Signorell <andri@signorell.net> }


\seealso{\code{\link{PlotPolar}}}
\examples{
Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid()

Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid(nr=0:5, ntheta=6)

Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid(ntheta=36, rlabels=NA, lblradians=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
