\name{CartToPol}
\alias{CartToPol}
\alias{PolToCart}
\alias{CartToSph}
\alias{SphToCart}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform Cartesian to Polar/Spherical Coordinates and Vice Versa
%%  ~~function to do ... ~~
}
\description{Transform cartesian into polar coordinates, resp. to spherical coordinates and vice versa.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CartToPol(x, y)
PolToCart(r, theta)

CartToSph(x, y, z, up = TRUE)
SphToCart(r, theta, phi, up = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, z}{vectors with the xy-coordianates to be transformed.
%%     ~~Describe \code{x} here~~
}
  \item{r}{a vector with the radius of the points.
%%     ~~Describe \code{x} here~~
}
  \item{theta}{a vector with the angle(s) of the points.
%%     ~~Describe \code{x} here~~
}
  \item{phi}{a vector with the angle(s) of the points.
%%     ~~Describe \code{x} here~~
}
  \item{up}{logical. If set to \code{TRUE} (default) theta is measured from x-y plane, else theta is measured from the z-axis.
%%     ~~Describe \code{x} here~~
}
}
\details{
Angles are in radians, not degrees (i.e., a right angle is pi/2). Use \code{\link{DegToRad}} to convert,
if you don't wanna do it by yourself.\cr
All parameters are recycled if necessary.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
PolToCart returns a list of x and y coordinates of the points.\cr
CartToPol returns a list of r for the radius and theta for the angles of the given points.
}
\author{
Andri Signorell <andri@signorell.net>, Christian W. Hoffmann <christian@echoffmann.ch>
%%  ~~who you are~~
}

\examples{
CartToPol(x=1, y=1)
CartToPol(x=c(1,2,3), y=c(1,1,1))
CartToPol(x=c(1,2,3), y=1)


PolToCart(r=1, theta=pi/2)
PolToCart(r=c(1,2,3), theta=pi/2)

CartToSph(x=1, y=2, z=3)   # r=3.741657, theta=0.930274, phi=1.107149
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
\keyword{dplot}
