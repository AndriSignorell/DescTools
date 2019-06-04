\name{ColToRgb}
\alias{ColToRgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Color to RGB Conversion

%%  ~~function to do ... ~~
}
\description{R color to RGB (red/green/blue) conversion.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ColToRgb(col, alpha = FALSE)
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
\details{This is merely a wrapper to \code{\link{col2rgb}}, defined in order to follow this package's naming conventions.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with a column for each color. The three rows of the matrix indicate red, green and blue value and are named "red", "green", and "blue" accordingly.
The matrix might have a 4th row if an alpha channel is requested.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{col2rgb}}, \code{\link{RgbToCol}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ColToRgb("peachpuff")
ColToRgb(c(blu = "royalblue", reddish = "tomato")) # names kept

ColToRgb(1:8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
