\name{PlotMultiDens}

\alias{PlotMultiDens}
\alias{PlotMultiDens.default}
\alias{PlotMultiDens.formula}

\title{Plot Multiple Density Curves
}
\description{Multiple density curves are plotted on the same plot. The function
plots the density curves in the defined colors and linetypes, after having calculated the
globally appropriate xlim- and ylim-values. A legend can directly be included.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMultiDens(x, ...)

\method{PlotMultiDens}{default}(x, xlim = NULL, ylim = NULL, col = Pal(), lty = "solid",
              lwd = 2, fill = NA, xlab = "x", ylab = "density", args.dens = NULL,
              args.legend = NULL, na.rm = FALSE, flipxy = FALSE, ...)

\method{PlotMultiDens}{formula}(formula, data, subset, na.action, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list of vectors whose densities are to be plotted. Use \code{\link{split}} to separate a vector by groups.
  (See examples)
%%     ~~Describe \code{x} here~~
}
  \item{xlim, ylim}{xlim, ylim of the plot.
%%     ~~Describe \code{xlim} here~~
}
  \item{col}{colors of the lines, defaults to \code{Pal()}, returning the default palette.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type of the lines.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line widths for the lines.
%%     ~~Describe \code{lwd} here~~
}
\item{fill}{colors for fill the area under the density curve. If set to \code{NA} (default) there will be no color.
}
 \item{xlab, ylab}{a title for the x, resp. y axis. Defaults to \code{"x"} and \code{"density"}.
}
  \item{args.dens}{list of additional arguments to be passed to the \code{density} function. \cr
  If set to \code{NULL} the defaults will be used. Those are \code{n = 4096} (2^12) and \code{kernel = "epanechnikov"}.
%%     ~~Describe \code{args.legend} here~~
}
  \item{args.legend}{list of additional arguments to be passed to the \code{legend} function.
  Use \code{args.legend = NA} if no legend should be added.
%%     ~~Describe \code{args.legend} here~~
}
  \item{na.rm}{should \code{NA}s be omitted? Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
  \item{flipxy}{logical, should x- and y-axis be flipped? Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
 \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
 \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{\dots}{the dots are passed to \code{plot(...)}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
All style arguments, density arguments and data list elements will be recycled if necessary.\cr
The argument \code{flipxy} leads to exchanged x- and y-values. This option can be used to plot density curves with a vertical orientation for displaying marginal densities.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data.frame with 3 columns, containing the \code{bw}, \code{n} and \code{kernel} parameters used for the list elements.
The number of rows correspond to the length of the list x.
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\note{Consider using: \cr
\preformatted{
  library(lattice)
  densityplot( ~ delivery_min | driver, data=d.pizza)
}

as alternative when not all curves should be plotted in the same plot.

}


\seealso{\code{\link{PlotViolin}}, \code{\link{density}}
}
\examples{
x <- rnorm(1000,0,1)
y <- rnorm(1000,0,2)
z <- rnorm(1000,2,1.5)

# the input of the following function MUST be a numeric list
PlotMultiDens(list(x=x,y=y,z=z))

# use area fill
PlotMultiDens(list(x=x,y=y,z=z), fill=SetAlpha(c("red","green","blue"), 0.4))


PlotMultiDens( x=split(d.pizza$delivery_min, d.pizza$driver), na.rm=TRUE
  , main="delivery time ~ driver", xlab="delivery time [min]", ylab="density"
  , lwd=1:7, lty=1:7
  , panel.first=grid())
# this example demonstrates the definition of different line types and -colors
# an is NOT thought as recommendation for good plotting practice... :-)


# the formula interface
PlotMultiDens(delivery_min ~ driver, data=d.pizza)

# recyling of the density parameters
res <- PlotMultiDens(x=split(d.pizza$temperature, d.pizza$driver),
              args.dens = list(bw=c(5,2), kernel=c("rect","epanechnikov")), na.rm=TRUE)
res

# compare bandwidths
PlotMultiDens(x=split(d.pizza$temperature, d.pizza$driver)[1],
                     args.dens = list(bw=c(1:5)), na.rm=TRUE,
                     args.legend=NA, main="Compare bw")
legend(x="topright", legend=gettextf("bw = \%s", 1:5), fill=rainbow(5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
