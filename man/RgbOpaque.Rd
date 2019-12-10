\name{ColToOpaque}
\alias{ColToOpaque}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Equivalent Opaque Color for Transparent Color 
%%  ~~function to do ... ~~
}
\description{Determine the equivalent opaque RGB color for a given partially transparent RGB color against a background of any color. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ColToOpaque(col, alpha = NULL, bg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{the color as hex value (use converters below if it's not available). \code{col} and \code{alpha} are recycled.
%%     ~~Describe \code{col} here~~
}
  \item{alpha}{the alpha channel, if left to NULL the alpha channels of the colors are used
%%     ~~Describe \code{alpha} here~~
}
  \item{bg}{the background color to be used to calculate against (default is "white")
%%     ~~Describe \code{bg} here~~
}
}
\details{Reducing the opacity against a white background is a good way to find usable lighter and less saturated tints of a base color. For doing so, we sometimes need to get the equivalent opaque color for the transparent color.
%%  ~~ If necessary, more details than the description above ~~
}
\value{An named vector with the hexcodes of the opaque colors.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{ColToHex}}, \code{\link{DecToHex}}, \code{\link{RgbToHex}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cols <- c(SetAlpha("limegreen", 0.4), ColToOpaque(ColToHex("limegreen"), 0.4), "limegreen")
barplot(c(1, 1.2, 1.3), col=cols, panel.first=abline(h=0.4, lwd=10, col="grey35"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot}% __ONLY ONE__ keyword per line
\keyword{ color}% __ONLY ONE__ keyword per line
