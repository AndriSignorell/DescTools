\name{ColToHsv}
\alias{ColToHsv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{R Color to HSV Conversion

%%  ~~function to do ... ~~
}
\description{ColToHsv transforms colors from R color into HSV space (hue/saturation/value). 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ColToHsv(col, alpha = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{vector of any of the three kind of R colors, i.e., either a color name (an element of \code{\link{colors}}()), 
  a hexadecimal string of the form "#rrggbb" or "#rrggbbaa", or an integer i meaning palette()[i]. 
  Non-string values are coerced to integer.

%%     ~~Describe \code{col} here~~
}
  \item{alpha}{logical value indicating whether alpha channel (opacity) values should be returned.
%%     ~~Describe \code{alpha} here~~
}
}
\details{Converts a color first into RGB an from there into HSV space by means of the functions \code{\link{rgb2hsv}} and \code{\link{col2rgb}}.

Value (brightness) gives the amount of light in the color.
Hue describes the dominant wavelength.
Saturation is the amount of Hue mixed into the color. 

An HSV colorspace is relative to an RGB colorspace, which in R is sRGB, which has an implicit gamma correction. 


%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with a column for each color. The three rows of the matrix indicate hue, saturation and value and are named "h", "s", and "v" accordingly. 


%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{rgb2hsv}}, \code{\link{ColToRgb}}
}
\examples{
ColToHsv("peachpuff")
ColToHsv(c(blu = "royalblue", reddish = "tomato")) # names kept

ColToHsv(1:8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color}
