\name{BarText}
\alias{BarText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add the Value Labels to a Barplot
%%  ~~function to do ... ~~
}
\description{Place text either in the middle of the stacked bars of a barplot or on top of a side by side barplot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BarText(height, b, labels = height, beside = FALSE, horiz = FALSE,
        cex = par("cex"), adj = NULL, top = TRUE, offset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{either a vector or matrix of values describing the bars which make up the plot exactly as used for creating the barplot.
%%     ~~Describe \code{height} here~~
}
  \item{b}{the returned mid points as returned by \code{b <- barplot(...)}.
%%     ~~Describe \code{b} here~~
}
  \item{labels}{the labels to be placed on the bars.
%%     ~~Describe \code{labels} here~~
}
  \item{beside}{a logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
%%     ~~Describe \code{beside} here~~
}
  \item{horiz}{a logical value. If FALSE, the bars are drawn vertically with the first bar to the left. If TRUE, the bars are drawn horizontally with the first at the bottom.
%%     ~~Describe \code{horiz} here~~
}
\item{cex}{numeric character expansion factor; multiplied by \code{\link{par}}\code{("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.}
\item{adj}{one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels. On most devices values outside that interval will also work.}

\item{top}{logical, defining if the labels should be placed on top of the bars (or above the bottom, if height is negative). Will be ignored if \code{beside} is \code{FALSE}.}

\item{offset}{a vector indicating how much the bars should be shifted relative to the x axis.}

\item{\dots}{the dots are passed to the \code{\link{text}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
It sometimes makes sense to place the data values directly on the bars. The x coordinates of the labels can be found by using \code{\link{barplot}()} result, if they are to be centered at the top of each bar. \code{BarText()} calculates the rest.
Notice that when the labels are placed on top of the bars, they may be clipped. This can be avoided by setting \code{xpd=TRUE}.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{help}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# simple vector
x <- c(453, 44, 56, 34)
b <- barplot(x)
BarText(x, b, x)

# more complicated
b <- barplot(VADeaths, horiz = FALSE, col=hblue, beside = TRUE)
BarText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8)
BarText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8, top=FALSE,
        col="white", font=2)

b <- barplot(VADeaths, horiz = TRUE, col=hblue, beside = TRUE)
BarText(VADeaths, b=b, horiz = TRUE, beside = TRUE, cex=0.8)

b <- barplot(VADeaths)
BarText(VADeaths, b=b)

b <- barplot(VADeaths, horiz = TRUE)
BarText(VADeaths, b=b, horiz = TRUE, col="red", cex=1.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
