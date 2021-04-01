\name{AUC}
\alias{AUC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Area Under the Curve
%%  ~~function to do ... ~~
}
\description{Calculate the area under the curve with a simple "trapezoid" algorithm and with a more elaborated spline approach. The curve must be given by vectors of xy-coordinates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AUC(x, y, from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), 
    method = c("trapezoid", "step", "spline"), 
    absolutearea = FALSE, subdivisions = 100, na.rm = FALSE, ...) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{the xy-points of the curve
%%     ~~Describe \code{x} here~~
}
  \item{method}{
  The type of interpolation. Can be \code{"trapezoid"} (default), \code{"step"} or \code{"spline"}.
  The value \code{"spline"} results in the area under the natural cubic spline interpolation.
%%     ~~Describe \code{type} here~~
}
  \item{from}{The value from where to start calculating the area under the
    curve. Defaults to the smallest x value. Must not be smaller that that.}
  
  \item{to}{The value from where to end the calculation of the area under the
    curve. Defaults to the greatest x value. Must not be larger than that.}
  
  \item{absolutearea}{logical, determines whether negative
    areas should be added to the total area under the curve.  By
    default the \code{AUC} function subtracts areas that have negative y values. Set \code{absolutearea=TRUE} to \bold{add} the absolute value of the negative areas to the total area.}
  
  \item{subdivisions}{an integer telling how many subdivisions should be used for integrate (for non-linear approximations). Ignored if \code{method} is not \code{spline}.}
  
  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. In this case only the complete.cases of \code{x} and \code{y} will be used. \code{na.rm} defaults to \code{FALSE}.}
  
  \item{\dots}{additional arguments passed on to approx (for linear approximations). In particular rule can be set to determine how values outside the range of x is handled.}

%%     ~~Describe \code{na.rm} here~~

}
\details{If method is set to \code{"trapezoid"} then the curve is formed by connecting all points by a direct line (composite trapezoid rule). If \code{"step"} is chosen then a stepwise connection of two points is used in the sense that the y-value of the lower x-value is used for the range up to the next x-point.


 For area under a spline interpolation,
  \code{AUC()} uses the \code{\link{splinefun}()} function in combination with \code{\link{integrate}()} to
  calculate a numerical integral. 
  
  The \code{AUC()} function can handle unsorted time
  values (by sorting \code{x}), missing observations, ties for the \code{x} values (by ignoring duplicates), and integrating over
  part of the area or even outside the area.
%%  ~~ If necessary, more details than the description above ~~

}
\value{
numeric value of the area under the curve.
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
