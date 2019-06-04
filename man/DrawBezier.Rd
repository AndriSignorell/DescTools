\name{DrawBezier}
\alias{DrawBezier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Bezier Curve
%%  ~~function to do ... ~~
}
\description{Draw a Bezier curve.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawBezier(x = 0, y = x, nv = 100, col = par("col"), lty = par("lty"), 
           lwd = par("lwd"), plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector of xy-coordinates to define the Bezier curve. Should at least contain 3 points.
%%     ~~Describe \code{x} here~~
}
  \item{nv}{number of vertices to draw the curve.
%%     ~~Describe \code{nv} here~~
}
  \item{col}{color(s) for the curve. Default is \code{par("fg")}. 
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
}
  \item{lwd}{line width for borders and shading.
}
  \item{plot}{logical. If \code{TRUE} the structure will be plotted. If \code{FALSE} only the xy-points are calculated and returned.
    Use this if you want to combine several geometric structures to a single polygon.
}
}
\details{
Bezier curves appear in such areas as mechanical computer aided design (CAD). 
They are named after P. Bezier, who used a closely related representation in Renault's UNISURF 
CAD system in the early 1960s (similar, unpublished, work was done by P. de Casteljau at Citroen 
in the late 1950s and early 1960s). The 1970s and 1980s saw a flowering of interest in Bezier curves, 
with many CAD systems using them, and many important developments in their theory. 
The usefulness of Bezier curves resides in their many geometric and analytical properties. 
There are elegant and efficient algorithms for evaluation, differentiation, 
subdivision of the curves, and conversion to other useful representations. (See: Farin, 1993)

%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{DrawBezier} invisibly returns a list of the calculated coordinates for all shapes. 
}

\references{
G. Farin (1993) \emph{Curves and surfaces for computer aided geometric design. A practical guide}, Acad. Press
}

\author{Frank E Harrell Jr <f.harrell@vanderbilt.edu>
}



\seealso{\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawCircle}}, \code{\link{DrawArc}}
}
\examples{
Canvas(xlim=c(0,1))
grid()
DrawBezier( x=c(0,0.5,1), y=c(0,0.5,0), col="blue", lwd=2)
DrawBezier( x=c(0,0.5,1), y=c(0,1,0), col="red", lwd=2)
DrawBezier( x=c(0,0.25,0.5,0.75,1), y=c(0,1,1,1,0), col="darkgreen", lwd=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
