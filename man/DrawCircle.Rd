\name{DrawCircle}
\alias{DrawCircle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Circle
%%  ~~function to do ... ~~
}
\description{Draw one or several circle on an existing plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawCircle(x = 0, y = x, r.out = 1, r.in = 0,
           theta.1 = 0, theta.2 = 2*pi, border = par("fg"),
           col = NA, lty = par("lty"), lwd = par("lwd"),
           nv = 100, plot = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector (or scalar) of xy-coordinates for the center(s) of the circle(s).
%%     ~~Describe \code{x} here~~
}
  \item{r.out}{a vector (or scalar) of the outer radius of the circle.
%%     ~~Describe \code{radius.out} here~~
}
  \item{r.in}{a vector (or scalar) of a potential inner radius of an annulus.
%%     ~~Describe \code{radius.in} here~~
}
  \item{theta.1}{a vector (or scalar) of the starting angle(s). The sectors are built counterclockwise.
%%     ~~Describe \code{angle.beg} here~~
}
  \item{theta.2}{a vector (or scalar) of the ending angle(s).
%%     ~~Describe \code{angle.end} here~~
}

  \item{nv}{number of vertices to draw the circle.
%%     ~~Describe \code{nv} here~~
}
  \item{border}{color for circle borders. The default is par("fg"). Use border = \code{NA}
to omit borders.
%%     ~~Describe \code{border} here~~
}
  \item{col}{color(s) to fill or shade the circle(s) with. The default \code{NA} (or also NULL) means
do not fill, i.e., draw transparent circles, unless density is specified.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width for borders and shading.
%%     ~~Describe \code{lwd} here~~
}
  \item{plot}{logical. If \code{TRUE} the structure will be plotted. If \code{FALSE} only the points are
calculated and returned. Use this option if you want to combine several geometric
structures to a polygon.
%%     ~~Describe \code{plot} here~~
}
}
\details{All geometric arguments will be recycled.
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

\seealso{\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawEllipse}}, \code{\link{DrawArc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Canvas(xlim = c(-5,5), xpd=TRUE)
cols <- Pal("Helsana")[1:4]

# Draw ring
DrawCircle (r.in = 1, r.out = 5, border="darkgrey", col=SetAlpha(hyellow, 0.2), lwd=2)

# Draw circle
DrawCircle (r.in = 6, border=hgreen, lwd=3)

# Draw sectors
geom <- rbind(c(-pi, 0, .25, .5), c(0, pi, 1, 2),
              c(-pi/2, pi/2, 2, 2.5), c(pi/2, 3 * pi/2, 3, 4),
              c(pi - pi/8, pi + pi/8, 1.5, 2.5))

DrawCircle (r.in = geom[,3], r.out = geom[,4],
           theta.1 = geom[,1], theta.2 = geom[,2],
           col = SetAlpha(cols, 0.6),
           border = cols, lwd=1)


# clipping
Canvas(bg="lightgrey", main="Yin ~ Yang")
DrawCircle (r.out = 1, col="white")
clip(0, 2, 2, -2)
DrawCircle(col="black")
clip(-2, 2, 2, -2)
DrawCircle (y = c(-0.5,0.5), r.out = 0.5, col=c("black", "white"), border=NA)
DrawCircle (y = c(-0.5,0.5), r.out = 0.1, col=c("white", "black"), border=NA)
DrawCircle ()


# overplotting circles
Canvas(xlim=c(-5,5))
DrawCircle (r.out=4:1, col=c("white", "steelblue2", "white", "red"), lwd=3, nv=300)


# rotation
x <- seq(-3, 3, length.out=10)
y <- rep(0, length.out=length(x))

Canvas(xlim=c(-5,5), bg="black")

sapply( (0:11) * pi/6, function(theta) {
  xy <- Rotate(x, y=y, theta=theta)
  DrawCircle (x=xy$x, y=xy$y, r.in=2.4, border=SetAlpha("white", 0.2))
} )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

