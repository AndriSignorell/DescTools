\name{ColToGrey}
\alias{ColToGrey}
\alias{ColToGray}
\title{Convert Colors to Grey/Grayscale}
\description{
  Convert colors to grey/grayscale so that you can see how your plot
  will look after photocopying or printing to a non-color printer.
}
\usage{
ColToGrey(col)
ColToGray(col)
}
\arguments{
  \item{col}{ vector of any of the three kind of R colors, i.e., either a color name (an element of colors()), 
  a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or an integer i meaning palette()[i]. 
  Non-string values are coerced to integer.
}
}
\details{
  Converts colors to greyscale using the formula grey = 0.3*red +
  0.59*green + 0.11*blue.  This allows you to see how your color plot
  will approximately look when printed on a non-color printer or photocopied.
}
\value{
  A vector of colors (greys) corresponding to the input colors.
}
\note{ These function was previously published as \code{Col2Grey()} in the  \pkg{TeachingDemos} package and has been integrated here without logical changes. 
}


\author{ Greg Snow <greg.snow@imail.org> }
\seealso{ \code{\link{grey}}, \code{\link{ColToRgb}}, dichromat package }
\examples{
par(mfcol=c(2,2))
tmp <- 1:3
names(tmp) <- c('red','green','blue')

barplot(tmp, col=c('red','green','blue'))
barplot(tmp, col=ColToGrey(c('red','green','blue')))

barplot(tmp, col=c('red','#008100','#3636ff'))
barplot(tmp, col=ColToGrey(c('red','#008100','#3636ff')))
}
\keyword{color}

