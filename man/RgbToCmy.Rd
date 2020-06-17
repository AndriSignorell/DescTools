\name{RgbToCmy}
\alias{RgbToCmy}
\alias{CmykToCmy}
\alias{CmyToCmyk}
\alias{RgbToCmy}
\alias{CmykToRgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conversion Between RGB and CMYK
%%  ~~function to do ... ~~
}
\description{These function convert colors between RGB and CMYK system.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RgbToCmy(col, maxColorValue = 1)
CmykToRgb(cyan, magenta, yellow, black, maxColorValue=1)
CmyToCmyk(col)
CmykToCmy(col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{the matrix of the color to be converted
%%     ~~Describe \code{col} here~~
}
  \item{cyan}{cyan values of the color(s) to be converted
%%     ~~Describe \code{col} here~~
}
  \item{magenta}{magenta values of the color(s) to be converted
%%     ~~Describe \code{col} here~~
}
  \item{yellow}{yellow values of the color(s) to be converted
%%     ~~Describe \code{col} here~~
}
  \item{black}{black values of the color(s) to be converted
%%     ~~Describe \code{col} here~~
}
  \item{maxColorValue}{ the value for the color
%%     ~~Describe \code{maxColorValue} here~~
}
}
\value{the converted value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{RgbToCol}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
CmykToRgb(0.42, 45.23, 85.14, maxColorValue=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
