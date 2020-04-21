\name{PlotFdist}
\alias{PlotFdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Frequency Distribution Plot
%%  ~~function to do ... ~~
}
\description{
This function is designed to give a univariate graphic representation of a numeric vectors frequency distribution.
It combines a histogram, a density curve, a boxplot and the empirical cumulative distribution function (ecdf) in one single plot. A rug as well as a model distribution curve (e.g. a normal curve) can optionally be superposed. This results in a dense and informative picture of the facts.
Still the function remains flexible as all possible arguments can be passed to the single components (\code{hist}, \code{boxplot} etc.) as a list (see examples).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotFdist(x, main = deparse(substitute(x)), xlab = "", xlim = NULL,
          args.hist = NULL, args.rug = NA, args.dens = NULL,
          args.curve = NA, args.boxplot = NULL, args.ecdf = NULL,
          args.curve.ecdf = NA, heights = NULL, pdist = NULL,
          na.rm = FALSE, cex.axis = NULL, cex.main = NULL, mar = NULL, las = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numerical variable, whose distribution is to be plotted.
%%     ~~Describe \code{x} here~~
}
  \item{main}{main title of the plot.
%%     ~~Describe \code{main} here~~
}
 \item{xlab}{label of the x-axis, defaults to \code{""}. (The name of the variable is typically  placed in the main title and would be redundant here.)
%%     ~~Describe \code{xlab} here~~
}
  \item{xlim}{range of the x-axis, defaults to a pretty \code{range(x, na.rm = TRUE)}.
%%     ~~Describe \code{xlim} here~~
}
  \item{args.hist}{list of additional arguments to be passed to the histogram \code{hist()}.
  The defaults chosen when setting \code{args.hist = NULL} are more or less the same as in \code{\link{hist}}. The argument \code{type} defines, whether a histogram (\code{"hist"}) or a plot with \code{type = "h"} (for 'histogram' like vertical lines for \code{mass} representation) should be used.
  The arguments for a "h-plot"" will be \code{col}, \code{lwd}, \code{pch.col}, \code{pch}, \code{pch.bg} for the line and for an optional point character on top.
  The default type used will be chosen on the structure of \code{x}. If \code{x} is an integer with up to 12 unique values there will be a "h-plot" and else a histogram!
%%     ~~Describe \code{args.hist} here~~
}
  \item{args.rug}{list of additional arguments to be passed to the function \code{rug()}.
      Use \code{args.rug = NA} if no rug should be added. This is the default. Use \code{args.rug = NULL} to add rug with reasonable default values.
  %%     ~~Describe \code{args.rug} here~~
}
  \item{args.dens}{list of additional arguments to be passed to \code{density}.
  Use \code{args.dens = NA} if no density curve should be drawn. The defaults are taken from \code{\link{density}}.
%%     ~~Describe \code{args.dens} here~~
}
  \item{args.curve}{list of additional arguments to be passed to \code{\link{curve}}.
  This argument allows to add a fitted distribution curve to the histogram. By default no curve will be added (\code{args.curve = NA}). If the argument is set to \code{NULL}, a normal curve with \code{mean(x)} and \code{sd(x)} will be drawn. See examples for more details.
%%     ~~Describe \code{args.dens} here~~
}
  \item{args.boxplot}{list of additional arguments to be passed to the boxplot \code{boxplot()}.
  The defaults are pretty much the same as in \code{\link{boxplot}}.
  The two additional arguments \code{pch.mean} (default \code{23}) and \code{col.meanci} (default \code{"grey80"}) control, if the mean is displayed within the boxplot. Setting those  arguments to \code{NA} will prevent them from being displayed.
%%     ~~Describe \code{args.hist} here~~
}
  \item{args.ecdf}{list of additional arguments to be passed to \code{ecdf()}.
  Use \code{args.ecdf = NA} if no empirical cumulation function should be included in the plot.
  The defaults are taken from \code{\link{plot.ecdf}}.
%%     ~~Describe \code{args.ecdf} here~~
}
  \item{args.curve.ecdf}{list of additional arguments to be passed to \code{\link{curve}}.
  This argument allows to add a fitted distribution curve to the cumulative distribution function. By default no curve will be added (\code{args.curve.ecdf = NA}). If the argument is set to \code{NULL}, a normal curve with \code{mean(x)} and \code{sd(x)} will be drawn. See examples for more details.
%%     ~~Describe \code{args.dens} here~~
}
  \item{heights}{heights of the plotparts, defaults to \code{c(2,0.5,1.4)} for the histogram, the boxplot
  and the empirical cumulative distribution function, resp. to \code{c(2,1.5)} for a histogram and a boxplot only.
%%     ~~Describe \code{heights} here~~
}
  \item{pdist}{distances of the plotparts, defaults to \code{c(0, 0)}, say there will be no distance between the histogram, the boxplot and the ecdf-plot. This can be useful for instance in case that the x-axis has to
  be added to the histogram.
%%     ~~Describe \code{heights} here~~
}
  \item{na.rm}{logical, should \code{NA}s be omitted? Histogram and boxplot could do without this option,
  but the density-function refuses to plot with missings. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
\item{cex.axis}{character extension factor for the axes.}
\item{cex.main}{character extension factor for the main title. Must be set in dependence of the plot parts in order to get a harmonic view.}

\item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of lines of outer margin to be specified on the four sides of the plot. The default is \code{c(0, 0, 3, 0)}.}

\item{las}{numeric in {0,1,2,3}; the style of axis labels. See \code{\link{par}}. }
}

\details{Performance has been significantly improved, but if \code{x} is growing large (n > 1e7) the function will take its time to complete. Especially the density curve and the ecdf, but as well as the boxplot (due to the chosen alpha channel) will take their time to calculate and plot.\cr
In such cases consider taking a sample, i.e. \code{  PlotFdist(x[sample(length(x), size=5000)])}, the big picture of the distribution won't usually change much.
.}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{hist}}, \code{\link{boxplot}}, \code{\link{ecdf}}, \code{\link{density}}, \code{\link{rug}}, \code{\link{layout}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE)

# define additional arguments for hist, dens and boxplot
# do not display the mean and its CI on the boxplot
PlotFdist(d.pizza$delivery_min, args.hist=list(breaks=50),
  args.dens=list(col="olivedrab4"), na.rm=TRUE,
  args.boxplot=list(col="olivedrab2", pch.mean=NA, col.meanci=NA))


# do a "h"-plot instead of a histogram for integers
x <- sample(runif(10), 100, replace = TRUE)
PlotFdist(x, args.hist=list(type="mass"))

pp <- rpois(n = 100, lambda = 3)
PlotFdist(pp, args.hist = list(type="mass", pch=21, col=horange,
          cex.pch=2.5, col.pch=hred, lwd=3, bg.pch="white"),
          args.boxplot = NULL, args.ecdf = NA, main="Probability mass function")

# special arguments for hist, density and ecdf
PlotFdist(x=faithful$eruptions,
          args.hist=list(breaks=20), args.dens=list(bw=.1),
          args.ecdf=list(cex=1.2, pch=16, lwd=1), args.rug=TRUE)

# no density curve, no ecdf but add rug instead, make boxplot a bit higher
PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE, args.dens=NA, args.ecdf=NA,
  args.hist=list(xaxt="s"),  # display x-axis on the histogram
  args.rug=TRUE, heights=c(3, 2.5), pdist=2.5, main="Delivery time")

# alpha channel on rug is cool, but takes its time for being drawn...
PlotFdist(x=d.pizza$temperature, args.rug=list(col=SetAlpha("black", 0.1)), na.rm=TRUE)

# plot a normal density curve, but no boxplot nor ecdf
x <- rnorm(1000)
PlotFdist(x, args.curve = NULL, args.boxplot=NA, args.ecdf=NA)

# compare with a t-distribution
PlotFdist(x, args.curve = list(expr="dt(x, df=2)", col="darkgreen"),
          args.boxplot=NA, args.ecdf=NA)
legend(x="topright", legend=c("kernel density", "t-distribution (df=2)"),
       fill=c(getOption("col1", hred), "darkgreen"), xpd=NA)

# add a gamma distribution curve to both, histogram and ecdf
ozone <- airquality$Ozone; m <- mean(ozone, na.rm = TRUE); v <- var(ozone, na.rm = TRUE)
PlotFdist(ozone, args.hist = list(breaks=15),
  args.curve = list(expr="dgamma(x, shape = m^2/v, scale = v/m)", col=hecru),
  args.curve.ecdf = list(expr="pgamma(x, shape = m^2/v, scale = v/m)", col=hecru),
  na.rm = TRUE, main = "Airquality - Ozone")

legend(x="topright", xpd=NA,
       legend=c(expression(plain("gamma:  ") * Gamma * " " * bgroup("(", k * " = " *
           over(bar(x)^2, s^2) * " , " * theta * plain(" = ") * over(s^2, bar(x)), ")") ),
                "kernel density"),
       fill=c(hecru, getOption("col1", hred)), text.width = 0.25)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

