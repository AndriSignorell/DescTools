\name{HexToRgb}
\alias{HexToRgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Hexstring Color to a Matrix With Three Red/Green/Blue Rows
%%  ~~function to do ... ~~
}
\description{
\code{HexToRgb()} converts a hexstring color the its red/green/blue representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HexToRgb(hex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hex}{a color or a vector of colors specified as hexadecimal string of the form "#RRGGBB" or "#RRGGBBAA"}
%%     ~~Describe \code{hex} here~~
}
\details{A hex color is written as a hash character, "#", followed by 3 or 4 hexadecimal numbers, say 6, resp. 8, digits (0-9A-F). The first 3 pairs of digits specify the red, green and blue components. When there are 8 digits, then the last pair is interpreted as alpha channel defining transparency, where \code{00} represents a fully transparent color and \code{FF} represent a fully opaque color.\cr
The result will be returned as a matrix having 3 or 4 rows, depending on if the input contained a \code{RRGGBBAA} definition or not. No distinction is made between upper and lower case. A missing leading # is tolerated.}
\value{a matrix with 3 or 4 rows.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{HexToCol}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
HexToRgb(c("#ADD8E6", "#FA1572"))

# 4-digit representation returns a 4 row matrix
HexToRgb(hex=c("#A52A2ABB","#A52A3B","C52A3B"))
}
\keyword{ color }
