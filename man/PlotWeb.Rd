\name{PlotWeb}
\alias{PlotWeb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Web of Connected Points
%%  ~~function to do ... ~~
}
\description{
This plot can be used to graphically display a correlation matrix by using the linewidth between the nodes
in proportion to the correlation of two variables. It will place the elements homogenously around a circle and draw
connecting lines between the points.
}
\usage{
PlotWeb(m, col = c(hred, hblue), lty = NULL, lwd = NULL, args.legend=NULL,
        pch = 21, pt.cex = 2, pt.col = "black", pt.bg = "darkgrey",
        cex.lab = 1, las = 1, adj = NULL, dist = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{a symmetric matrix of numeric values
%%     ~~Describe \code{m} here~~
}
  \item{col}{the color for the connecting lines
%%     ~~Describe \code{col} here~~
}
  \item{lty}{the line type for the connecting lines, the default will be \code{par("lty")}.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{the line widths for the connecting lines. If left to \code{NULL} it will be linearly scaled between the minimum and maximum value of \code{m}.
%%     ~~Describe \code{lty} here~~
}

  \item{args.legend}{list of additional arguments to be passed to the \code{legend} function.
  Use \code{args.legend = NA} if no legend should be added.
%%     ~~Describe \code{args.legend} here~~
}
  \item{pch}{the plotting symbols appearing in the plot, as a non-negative numeric vector (see \code{\link{points}}, but unlike there negative values are omitted) or a vector of 1-character strings, or one multi-character string.
}
  \item{pt.cex}{expansion factor(s) for the points.
}
  \item{pt.col}{the foreground color for the points, corresponding to its argument \code{col}.
}
  \item{pt.bg}{the background color for the points, corresponding to its argument \code{bg}.
}
 \item{las}{alignment of the labels, 1 means horizontal, 2 radial and 3 vertical.
%%     ~~Describe \code{labels} here~~
}
 \item{adj}{adjustments for the labels. (Left: 0, Right: 1, Mid: 0.5)
%%     ~~Describe \code{labels} here~~
}
\item{dist}{gives the distance of the labels from the outer circle. Default is 2.}
\item{cex.lab}{the character extension for the labels.
%%     ~~Describe \code{cex.lab} here~~
}
  \item{\dots}{dots are passed to plot.
%%     ~~Describe \code{\dots} here~~
}
}

\details{The function uses the lower triangular matrix of \code{m}, so this is the order colors, linewidth etc. must be given, when the defaults are to be overrun.}
\value{
A list of x and y coordinates, giving the coordinates of all the points drawn, useful for adding other elements to the plot.
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{PlotCorr}}
}
\examples{
m <- cor(d.pizza[, which(sapply(d.pizza, IsNumeric, na.rm=TRUE))[-c(1:2)]],
         use="pairwise.complete.obs")
PlotWeb(m=m, col=c(hred, hblue), main="Pizza Correlation")


# let's describe only the significant corrs and start with a dataset
d.m <- d.pizza[, which(sapply(d.pizza, IsNumeric, na.rm=TRUE))[-c(1:2)]]

# get the correlation matrix
m <- cor(d.m, use="pairwise.complete.obs")

# let's get rid of all non significant correlations
ctest <- PairApply(d.m,  function(x, y) cor.test(x, y)$p.value, symmetric=TRUE)

# ok, got all the p-values, now replace > 0.05 with NAs
m[ctest > 0.05] <- NA
# How does that look like now?
Format(m, na.form = ". ", leading="drop", digits=3, align = "right")

PlotWeb(m, las=2, cex=1.2)

# define line widths
PlotWeb(m, lwd=abs(m[lower.tri(m)] * 10))
}
\keyword{ hplot }
\keyword{ multivariate }
