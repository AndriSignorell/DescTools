\name{Rotate}
\alias{Rotate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rotate a Geometric Structure
%%  ~~function to do ... ~~
}
\description{Rotate a geometric structure by an angle theta around a centerpoint xy.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rotate(x, y = NULL, mx = NULL, my = NULL, theta = pi/3, asp = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors containing the coordinates of the vertices of the polygon
  , which has to be rotated.  The coordinates can be passed in a plotting structure (a list with x and y components), a two-column matrix, .... See \code{\link{xy.coords}}.
%%     ~~Describe \code{x} here~~
}
  \item{mx, my}{xy-coordinates of the center of the rotation. If left to NULL, the centroid of the structure will be used.
%%     ~~Describe \code{mx} here~~
}
  \item{theta}{angle of the rotation
%%     ~~Describe \code{theta} here~~
}
\item{asp}{the aspect ratio for the rotation. Helpful for rotate structures along an ellipse. }
}
\value{The function invisibly returns a list of the coordinates for the rotated shape(s).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawEllipse}}, \code{\link{DrawArc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
# let's have a triangle
Canvas(main="Rotation")
x <- DrawRegPolygon(nv=3)[[1]]

# and rotate
sapply( (0:3) * pi/6, function(theta) {
  xy <- Rotate( x=x, theta=theta )
  polygon(xy, col=SetAlpha("blue", 0.2))
} )

abline(v=0, h=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
