\name{SetAlpha}
\alias{SetAlpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add an Alpha Channel To a Color
%%  ~~function to do ... ~~
}
\description{ Add transparency to a color defined by its name or number. The function first converts the
 color to RGB and then appends the alpha channel.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SetAlpha(col, alpha = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{vector of two kind of R colors, i.e., either a color name (an element of \code{colors()}) or an integer i meaning \code{palette()[i]}.
%%     ~~Describe \code{col} here~~
}
  \item{alpha}{the alpha value to be added. This can be any value from 0 (fully transparent) to 1 (opaque). \code{NA} is interpreted so as to delete a potential alpha channel. Default is 0.5.
%%     ~~Describe \code{alpha} here~~
}
}
\details{All arguments are recyled as necessary.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Vector with the same length as \code{col}, giving the rgb-values extended by the alpha channel as hex-number (#rrggbbaa).
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{ColToHex}}, \code{\link{col2rgb}}

}
\examples{
SetAlpha("yellow", 0.2)
SetAlpha(2, 0.5)   # red

Canvas(3)
DrawCircle(x=c(-1,0,1), y=c(1,-1,1), r.out=2, col=SetAlpha(2:4, 0.4))

x <- rnorm(15000)
par(mfrow=c(1,2))
plot(x, type="p", col="blue" )
plot(x, type="p", col=SetAlpha("blue", .2), main="Better insight with alpha channel" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }

