\name{PlotMarDens}
\alias{PlotMarDens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scatterplot With Marginal Densities
%%  ~~function to do ... ~~
}
\description{Draw a scatter plot with marginal densities on the x- and y-axis. Groups can be defined by grp.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMarDens(x, y, grp = 1, xlim = NULL, ylim = NULL,
            col = rainbow(nlevels(factor(grp))),
            mardens = c("all","x","y"), pch = 1, pch.cex = 1,
            main = "", na.rm = FALSE, args.legend = NULL,
            args.dens = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of x values.
}
  \item{y}{numeric vector of y values (of same length as x).
}
  \item{grp}{grouping variable(s), typically factor(s), all of the same length as x.
}
  \item{xlim}{the x limits of the plot.
}
  \item{ylim}{the y limits of the plot.
}
  \item{col}{the colors for lines and points. Uses \code{rainbow()} colors by default.
}
 \item{mardens}{which marginal densities to plot. Can be set to either just x or y, or both (\code{"all"}, latter being the default).
}
  \item{pch}{a vector of plotting characters or symbols.
}
  \item{pch.cex}{magnification to be used for plotting characters relative to the current setting of cex.
}
  \item{main}{a main title for the plot, see also \link{title}.
}
  \item{na.rm}{logical, should NAs be omitted? Defaults to \code{FALSE}.
}
   \item{args.legend}{list of additional arguments for the legend. \code{args.legend} set to \code{NA} prevents a legend from being drawn.
}
  \item{args.dens}{list of additional arguments to be passed to \code{density}.
  Use \code{args.dens = NA} if no density curve should be drawn. The defaults are taken from \code{\link{density}}.
%%     ~~Describe \code{args.dens} here~~
}
  \item{\dots}{further arguments are passed to the function \code{plot()}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}


\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{plot}}, \code{\link{points}}, \code{\link{density}}, \code{\link{layout}}
}
\examples{
# best seen with: x11(7.5, 4.7)

# just one variable with marginal densities
PlotMarDens( y=d.pizza$temperature, x=d.pizza$delivery_min, grp=1
             , xlab="delivery_min", ylab="temperature", col=SetAlpha("brown", 0.4)
             , pch=15, lwd=3
             , panel.first= grid(), args.legend=NA
             , main="Temp ~ delivery"
)

# use a group variable
PlotMarDens( y=d.pizza$temperature, x=d.pizza$delivery_min, grp=d.pizza$area
  , xlab="delivery_min", ylab="temperature", col=c("brown","orange","lightsteelblue")
  , panel.first=list( grid() )
  , main = "temperature ~ delivery_min | area"
)
# reset layout
par(mfrow=c(1,1))
}
\keyword{ hplot }
