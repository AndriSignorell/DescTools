\name{PlotMosaic}
\alias{PlotMosaic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mosaic Plots
%%  ~~function to do ... ~~
}
\description{Plots a mosaic on the current graphics device.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMosaic(x, main = deparse(substitute(x)), horiz = TRUE, cols = NULL,
           off = 0.02, mar = NULL, xlab = NULL, ylab = NULL,
           cex = par("cex"), las = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a contingency table in array form, with optional category labels specified in the dimnames(x) attribute. The table is best created by the table() command. So far only 2-way tables are allowed.
%%     ~~Describe \code{x} here~~
}
  \item{main}{character string for the mosaic title.
%%     ~~Describe \code{main} here~~
}
  \item{horiz}{logical, defining the orientation of the mosaicplot. \code{TRUE} (default) makes a horizontal plot.
%%     ~~Describe \code{horiz} here~~
}
  \item{cols}{the colors of the plot.
%%     ~~Describe \code{cols} here~~
}
  \item{off}{the offset between the rectangles. Default is 0.02.
%%     ~~Describe \code{off} here~~
}
  \item{mar}{the margin for the plot.
%%     ~~Describe \code{mar} here~~
}
  \item{xlab, ylab}{x- and y-axis labels used for the plot; by default, the first and second element of names(dimnames(X)) (i.e., the name of the first and second variable in X).
%%     ~~Describe \code{xlab} here~~
}
 \item{cex}{numeric character expansion factor; multiplied by \code{par("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to 1.0.
%%     ~~Describe \code{cex} here~~
}
  \item{las}{the style of axis labels. 0 - parallel to the axis, 1 - horizontal, 2 - perpendicular, 3 - vertical.
%%     ~~Describe \code{las} here~~
}

 \item{\dots}{additional arguments are passed to the text function.
%%     ~~Describe \code{\dots} here~~
}

}
\details{The reason for this function to exist are the unsatisfying labels in base mosaicplot.
%%  ~~ If necessary, more details than the description above ~~
}
\value{list with the midpoints of the rectangles
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Friendly, M. (1994) Mosaic displays for multi-way contingency tables. \emph{Journal of the American Statistical Association}, \bold{89}, 190-200.


%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{mosaicplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotMosaic(HairEyeColor[,,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
