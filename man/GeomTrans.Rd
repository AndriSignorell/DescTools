\name{GeomTrans}
\alias{GeomTrans}
\docType{data}
\title{Geometric Transformations
%%   ~~ data name/kind ... ~~
}
\description{This function transforms geometric structures by translating, scaling and/or rotating them.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
GeomTrans(x, y = NULL, trans = 0, scale = 1, theta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors containing the coordinates of the vertices of the polygon
  , which has to be transformed.  The coordinates can be passed in a plotting structure (a list with x and y components), a two-column matrix, .... See \code{\link{xy.coords}}.
%%     ~~Describe \code{x} here~~
}
  \item{trans}{a vector of two values for the translation in x-, resp. y-direction. If only one value is supplied it will be recycled.
%%     ~~Describe \code{mx} here~~
}
  \item{scale}{a vector of two values for the scaling factor in x-, resp. y-direction. If only one value is supplied it will be recycled.
%%     ~~Describe \code{mx} here~~
}
 \item{theta}{angle of the rotation in radians starting from 3 o'clock counterclockwise.
%%     ~~Describe \code{theta} here~~
}

}

\value{The function invisibly returns a list of the coordinates for the transformed shape(s).
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

xt <- GeomTrans(x, trans=c(1, 3), scale=c(2, 2), theta=pi/4)
polygon(xt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
