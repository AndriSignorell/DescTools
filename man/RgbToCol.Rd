\name{RgbToCol}
\alias{RgbToCol}
\alias{RgbToLong}
\alias{LongToRgb}

\title{Find the Nearest Named R-Color to a Given RGB-Color }

\description{Converting a RGB-color to a named R-Color means looking for a color in the R-palette, which is nearest to the given RGB-color.
This function uses the minimum of squared distance (\code{"euclidean"}) or the minimum absolute distance (\code{"manhattan"}) as proximity measure. \cr
\code{RgbToLong()} converts a RGB-color to a long integer in numeric format. \code{LongToRGB()} does it the other way round.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RgbToCol(col, method = "rgb", metric = "euclidean")

RgbToLong(col)
LongToRgb(col)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{col}{the color in rgb code, say a matrix with the red, green and blue code in the rows.  }
    \item{method}{character string specifying the color space to be used. Can be \code{"rgb"} (default) or \code{"hsv"}.}
    \item{metric}{character string specifying the metric to be used for calculating distances between the colors.
        Available options are \code{"euclidean"} (default) and \code{"manhattan"}. Euclidean distances are root sum-of-squares of
        differences, and manhattan distances are the sum of absolute differences.}

}
\details{It may not be clear from the start which method, rgb or hsv, yield the more natural results. Trying and comparing is a recommended strategy. Moreover the shortest numerical distance will not always be the best choice, when comparing the colours visually.}
\value{the name of the nearest found R color.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{ColToRgb}} and the other conversion functions
}
\examples{
RgbToCol(matrix(c(162,42,42), nrow=3))

RgbToLong(matrix(c(162,42,42), nrow=3))
}
\keyword{ color }
