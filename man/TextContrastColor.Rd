\name{TextContrastColor}
\alias{TextContrastColor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choose Textcolor Depending on Background Color
%%  ~~function to do ... ~~
}
\description{Text of a certain color when viewed against certain backgrounds can be hard to see.
\code{TextContrastColor} returns either black or white depending on which has the better contrast.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TextContrastColor(col, method = c("glynn", "sonego"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{vector of any of the three kind of R colors, i.e., either a color name (an element of colors()),
  a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or an integer i meaning palette()[i].
  Non-string values are coerced to integer.
%%     ~~Describe \code{color} here~~
}
 \item{method}{defines the algorithm to be used. Can be one out of \code{"glynn"} or \code{"sonego"}. See details.
%%     ~~Describe \code{color} here~~
}
}
\details{
A simple heuristic in defining a text color for a given background color, is to pick the one that
is "farthest" away from "black" or "white".
The way Glynn chooses to do this is to compute the color intensity,
defined as the mean of the RGB triple, and pick "black" (intensity 0) for text color if the background
intensity is greater than 127, or "white" (intensity 255) when the background intensity is less than or equal to 127.
Sonego calculates \code{L <- c(0.2, 0.6, 0) \%*\% col2rgb(color)/255} and returns #000060 if L >= 0.2 and #FFFFA0 else.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector containing the contrast color (either black or white)
}
\references{
\url{http://research.stowers.org/mcm/efg/R/Color/Chart/index.htm}

(Reference for Sonego??)
}

\author{Andri Signorell <andri@signorell.net> based on code of
Earl F. Glynn, Stowers Institute for Medical Research, 2004
}

\examples{
# works fine for grays
PlotArea( y=matrix(rep(1, times=3, each=8), ncol=8), x=1:3,
  col=gray(1:8 / 8), ylab="", xlab="", axes=FALSE )
text( x=2, y=1:8-0.5, levels(d.pizza$driver),
  col=TextContrastColor(gray(1:8 / 8)))

# and not so fine, but still ok, for colors
par(mfrow=c(1,2))
PlotArea( y=matrix(rep(1, times=3, each=12), ncol=12), x=1:3,
  col=rainbow(12), ylab="", xlab="", axes=FALSE, main="method = Glynn" )
text( x=2, y=1:12-0.5, levels(d.pizza$driver),
  col=TextContrastColor(rainbow(12)))

PlotArea( y=matrix(rep(1, times=3, each=12), ncol=12), x=1:3,
  col=rainbow(12), ylab="", xlab="", axes=FALSE, main="method = Sonego" )
text( x=2, y=1:12-0.5, levels(d.pizza$driver),
  col=TextContrastColor(rainbow(12), method="sonego"))

}
\keyword{color}
