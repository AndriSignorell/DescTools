\name{HexToCol}
\alias{HexToCol}

\title{Identify Closest Match to a Color Given by a Hexadecimal String}
\description{
 Given a color as a hex string #rrggbb, find the closest match in the
 table of known (named) colors.
}
\usage{
HexToCol(hexstr, method = "rgb", metric = "euclidean")
}
\arguments{
  \item{hexstr}{a color or a vector of colors specified as hexadecimal string of the form "#RRGGBB" or "#RRGGBBAA"}
  \item{method}{character string specifying the color space to be used. Can be "rgb" (default) or "hsv".}
   \item{metric}{character string specifying the metric to be used for calculating distances between the colors.
Available options are "euclidean" (default) and "manhattan". Euclidean distances are root sum-of-squares of differences, and manhattan distances are the sum of absolute differences.}

}
\details{
Finds the color with the minimum squared distance in RGB space.
}
\value{
The colorname(s) of the closest match(es) (if more than one).
}
\author{Ben Bolker, vector support Andri Signorell <andri@signorell.net> }

\seealso{\link{ColToHex}, \link{ColToRgb}, \link{colors}}

\examples{
ColToHex(c("lightblue", "salmon"))

HexToCol(c("#ADD8E6", "#FA1572"))
HexToCol(Pal("Helsana"))

x <- ColToRgb("darkmagenta")
x[2,] <- x[2,] + 155
RgbToCol(x)
}
\keyword{color}
