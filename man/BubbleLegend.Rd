\name{BubbleLegend}
\alias{BubbleLegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add a Legend to a Bubble Plot
%%  ~~function to do ... ~~
}
\description{Add a legend for bubbles to a bubble plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BubbleLegend(x, y = NULL, area, cols, labels = NULL, cols.lbl = "black",
             width = NULL, xjust = 0, yjust = 1, inset = 0, border = "black",
             frame = TRUE, adj = c(0.5, 0.5), cex = 1, cex.names = 1,
             bg = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{the left x-coordinate to be used to position the legend. See 'Details'. }

  \item{y}{the top y-coordinate to be used to position the legend. See 'Details'. }

  \item{area}{the area(s) for the bubbles in bubble legend.}
  \item{cols}{the color appearing in the legend.}

  \item{labels}{a vector of labels to be placed at the right side of the legend.}
  \item{cols.lbl}{the textcolor for the labels of the bubbles.}

  \item{width}{the width of the legend.}
  \item{xjust}{how the legend is to be justified relative to the legend x location.
    A value of 0 means left justified, 0.5 means centered and 1 means right justified. }

  \item{yjust}{the same as \code{xjust} for the legend y location. }


  \item{inset}{inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword. }

  \item{border}{defines the bordor color of each rectangle. Default is none (\code{NA}).}

  \item{frame}{defines the bordor color of the frame around the whole legend. Default is none (\code{NA}). }

  \item{adj}{text alignment, horizontal and vertical. }

  \item{cex}{extension factor for the area, default 1.0. }
  \item{cex.names}{character extension for the labels, default 1.0. }

\item{bg}{the background color for the bubble legend.}

\item{\dots}{further arguments are passed to the function \code{text}. }

}
\details{
The labels are placed in the middle of the legend.
\cr\cr
The location of the legend may be specified by setting x to a single keyword from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"}. This places the legend on the inside of the plot frame at the given location. Partial argument matching is used. The optional inset argument specifies how far the legend is inset from the plot margins. If a single value is given, it is used for both margins; if two values are given, the first is used for x- distance, the second for y-distance. This is the same behaviour as it's implemented in \code{\link{legend}}.

%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{legend}}, \code{\link{FindColor}}, \code{\link{legend}}
}
\examples{
PlotBubble(x=d.pizza$delivery_min, y=d.pizza$temperature, area=d.pizza$price,
           xlab="delivery time", ylab="temperature",
           col=SetAlpha(as.numeric(d.pizza$area)+2, .5), border="darkgrey",
           na.rm=TRUE, main="Price-Bubbles", panel.first=grid())

BubbleLegend("bottomleft", area=c(1500, 1000, 500), frame=TRUE,
             cols=SetAlpha("steelblue",0.5), bg="green",
             labels=c(1500, 1000, 500), cex=0.8,
             cols.lbl=c("yellow", "red","blue"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot }
