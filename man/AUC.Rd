\name{AUC}
\alias{AUC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Area Under the Curve
%%  ~~function to do ... ~~
}
\description{Calculate the area under the curve with a naive algorithm and with a more elaborated spline approach. The curve must be given by vectors of xy-coordinates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AUC(x, y, method = c("trapezoid", "step", "spline"), na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{the xy-points of the curve
%%     ~~Describe \code{x} here~~
}
  \item{method}{can be \code{"trapezoid"} (default), \code{"step"} or \code{"spline"}.
%%     ~~Describe \code{type} here~~
}
  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. In this case only the complete.cases of x and y will be used. \code{na.rm} defaults to \code{FALSE}. 
%%     ~~Describe \code{na.rm} here~~
}
}
\details{If method is set to \code{"trapezoid"} then the curve is formed by connecting all points by a direct line (composite trapezoid rule). If \code{"step"} is chosen then a stepwise connection of two points is used.
For calculating the area under a spline interpolation the \code{\link{splinefun}} function  is used in combination
  with \code{\link{integrate}}. \cr
The AUC function will handle unsorted x values (by sorting x) and ties for the x values (by ignoring duplicates).
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Numeric value of the area under the curve.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>, spline part by Claus Ekstrom <claus@rprimer.dk>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{integrate}}, \code{\link{splinefun}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
AUC(x=c(1,3), y=c(1,1))

AUC(x=c(1,2,3), y=c(1,2,4), method="trapezoid")
AUC(x=c(1,2,3), y=c(1,2,4), method="step")

plot(x=c(1,2,2.5), y=c(1,2,4), type="l", col="blue", ylim=c(0,4))
lines(x=c(1,2,2.5), y=c(1,2,4), type="s", col="red")

x <- seq(0, pi, length.out=200)
AUC(x=x, y=sin(x)) 
AUC(x=x, y=sin(x), method="spline") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
