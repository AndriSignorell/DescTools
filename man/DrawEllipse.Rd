\name{DrawEllipse}
\alias{DrawEllipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw an Ellipse
%%  ~~function to do ... ~~
}
\description{Draw one or several ellipses on an existing plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawEllipse(x = 0, y = x, radius.x = 1, radius.y = 0.5, rot = 0,
            nv = 100, border = par("fg"), col = par("bg"),
            lty = par("lty"), lwd = par("lwd"), plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{the x and y co-ordinates for the centre(s) of the ellipse(s).
%%     ~~Describe \code{x} here~~
}
  \item{radius.x}{a scalar or a vector giving the semi-major axis of the ellipse.
%%     ~~Describe \code{radius.x} here~~
}
  \item{radius.y}{a scalar or a vector giving the semi-minor axis of the ellipse.
%%     ~~Describe \code{radius.y} here~~
}
  \item{rot}{angle of rotation in radians.
%%     ~~Describe \code{rot} here~~
}
  \item{nv}{number of vertices to draw the ellipses.
%%     ~~Describe \code{nv} here~~
}
  \item{border}{color for borders. The default is \code{par("fg")}. Use \code{border = NA} to omit borders.

%%     ~~Describe \code{border} here~~
}
  \item{col}{color(s) to fill or shade the annulus sector with. The default \code{NA} (or also \code{NULL})
means do not fill (say draw transparent).
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width for borders and shading.
%%     ~~Describe \code{lwd} here~~
}
  \item{plot}{logical. If \code{TRUE} the structure will be plotted. If \code{FALSE} only the points are
calculated and returned. Use this if you want to combine several geometric
structures to a single polygon.
%%     ~~Describe \code{plot} here~~
}
}
\details{
Use \code{\link{DegToRad}} if you want to define rotation angle in degrees.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function invisibly returns a list of the calculated coordinates for all shapes.
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

\seealso{\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawCircle}}, \code{\link{DrawArc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
par(mfrow=c(1,2))

Canvas()
DrawEllipse(rot = c(1:3) * pi/3, col=SetAlpha(c("blue","red","green"), 0.5) )


plot(cars)
m <- var(cars)
eig <- eigen(m)
eig.val <- sqrt(eig$values)
eig.vec <- eig$vectors

DrawEllipse(x=mean(cars$speed), y=mean(cars$dist), radius.x=eig.val[1] , radius.y=eig.val[2]
  , rot=acos(eig.vec[1,1]), border="blue", lwd=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

