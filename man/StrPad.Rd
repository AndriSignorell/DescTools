\name{StrPad}
\alias{StrPad}
\title{Pad a String With Justification}
\description{
  \code{StrPad} will fill a string x with defined characters to fit a given length.
}
\usage{
StrPad(x, width = NULL, pad = " ", adj = "left")}
\arguments{
  \item{x}{a vector of strings to be padded.}
  \item{width}{resulting width of padded string. If x is a vector and width is left to NULL, it will be set to the length of the largest string in x.}
  \item{pad}{string to pad with. Will be repeated as often as necessary. Default is " ".}
  \item{adj}{adjustement of the old string, one of \code{"left"}, \code{"right"}, \code{"center"}. If set to \code{"left"} the old string will be adjusted on the left and the new characters will be filled in on the right side.}
}
\value{
the string
}
\details{
If a string x has more characters than width, it will be chopped on the length of width.
}

\examples{
StrPad("My string", 25, "XoX", "center")
 # [1] "XoXXoXXoMy stringXXoXXoXX"
}
\author{
Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>\cr
some extensions Andri Signorell <andri@signorell.net>
}
\keyword{character}
\concept{ string }
