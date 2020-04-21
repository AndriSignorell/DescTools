\name{ColToHex}
\alias{ColToHex}
\alias{RgbToHex}
\title{Convert a Color or a RGB-color Into Hex String
}
\description{Convert a color given by name, by its palette index or by rgb-values into a string of the form "#rrggbb" or "#rrggbbaa".
}
\usage{
ColToHex(col, alpha = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{vector of any of either a color name (an element of colors()),
  or an integer i meaning palette()[i].
  Non-string values are coerced to integer.}
  \item{alpha}{the alpha value to be used. This can be any value from 0 (fully transparent) to 1 (opaque). Default is 1.}

}
\value{
Returns the colorvalue in #rrggbb" or #rrggbbaa" format. (character)
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\link{HexToCol}, \link{ColToRgb}, \link{colors}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ColToHex(c("lightblue", "salmon"))

x <- ColToRgb("darkmagenta")
x[2,] <- x[2,] + 155
RgbToCol(x)

RgbToHex(c(255,0,0))
}
\keyword{color }
