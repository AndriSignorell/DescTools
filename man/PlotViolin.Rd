\name{PlotViolin}
\alias{PlotViolin}
\alias{PlotViolin.default}
\alias{PlotViolin.formula}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Plot Violins Instead of Boxplots }
\description{
  This function serves the same utility as side-by-side boxplots, only
  it provides more detail about the different distribution. It
  plots violins instead of boxplots. That is, instead of a box, it
  uses the density function to plot the density. For skewed
  distributions, the results look like "violins". Hence the name.
}

\usage{
PlotViolin(x, ...)

\method{PlotViolin}{default}(x, ..., horizontal = FALSE, bw = "SJ", na.rm = FALSE, 
           names = NULL, args.boxplot = NULL) 

\method{PlotViolin}{formula}(formula, data, subset, na.action, ...) 
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Either a sequence of variable names, or a data frame, or a model formula}
  \item{horizontal}{logical indicating if the densityplots should be horizontal; default \code{FALSE} means vertical arrangement.}
  \item{bw}{the smoothing bandwidth (method) being used by \code{\link{density}}. \cr
    \code{bw} can also be a character string giving a rule to choose the bandwidth. See \code{\link{bw.nrd}}. 
    The default, has been switched from \code{"nrd0"} to \code{"SJ"}, following the general recommendation in Venables & Ripley (2002). \cr
    In case of a method, the average computed bandwidth is used.}
  \item{na.rm}{logical, should NAs be omitted? The density-function can't do with missings. Defaults to \code{FALSE}.}
  \item{names}{a vector of names for the groups.}
  \item{formula}{a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor).}
  \item{data}{a data.frame (or list) from which the variables in formula should be taken.}
  \item{subset}{an optional vector specifying a subset of observations to be used for plotting.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}  
\item{\dots}{The dots are passed to \code{\link{polygon}}. Notably, you can set the color to red with \code{col="red"}, and a border color with \code{border="blue"}}
  \item{args.boxplot}{list of arguments for a boxplot to be superposed to the densityplot. By default (NULL) a 
  black boxplot will be drawn. Set this to \code{NA} to suppress the boxplot.}

}
\value{
  If a boxplot was drawn then the function returns a list with the following components:
  \item{stats}{a matrix, each column contains the extreme of the lower
    whisker, the lower hinge, the median, the upper hinge and the
    extreme of the upper whisker for one group/plot.  If all the inputs
    have the same class attribute, so will this component.}
  \item{n}{a vector with the number of observations in each group.}
  \item{conf}{a matrix where each column contains the lower and upper
    extremes of the notch.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers.}
  \item{group}{a vector of the same length as \code{out} whose elements
    indicate to which group the outlier belongs.}
  \item{names}{a vector of names for the groups.}
}

\note{ This function is based on \code{\link[UsingR]{violinplot}} (package \pkg{UsingR}). 
Some adaptions were made in the interface, such as to accept the same
arguments as \code{\link{boxplot}} does. Moreover the function was extended by the option to have a boxplot superposed.

}

\references{ The code is based on the boxplot function from R/base.}
\author{ John Verzani, Andri Signorell <andri@signorell.net>}

\seealso{ \code{\link{boxplot}}, \code{\link{PlotMultiDens}} , \code{\link{density}} }

\examples{
# make a "violin"
x <- c(rnorm(100), rnorm(50,5))

PlotViolin(x, col = "brown")

par(mfrow=c(1,2))
f <- factor(rep(1:5, 30))
# make a quintet. Note also choice of bandwidth
PlotViolin(x ~ f, col = SetAlpha("steelblue",0.3), bw = "SJ", main="Vertical")

# and the same, but in horizontal arrangement
PlotViolin(x ~ f, col = SetAlpha("steelblue",0.3), bw = "SJ", horizontal = TRUE, 
  las=1, main="Horizontal")


# example taken from boxplot
boxplot(count ~ spray, data = InsectSprays, col = "lightgray", main="Boxplot")
PlotViolin(count ~ spray, data = InsectSprays, col = "lightgray", main="Violinplot")


# groupwise densityplots defined the same way as in boxplot
boxplot(len ~ supp*dose, data = ToothGrowth,
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg", ylab = "tooth length",
        col=c("yellow", "orange"), lty=c(1,2)
)

b <- PlotViolin(len ~ supp*dose, data = ToothGrowth,
           main = "Guinea Pigs' Tooth Growth",
           xlab = "Vitamin C dose mg", ylab = "tooth length",
           col=c("yellow", "orange"), lty=c(1,2)
)
# use points, if the medians deserve special attention
points(x=1:6, y=b$stats[3,], pch=21, bg="white", col="black", cex=1.2)
}

\keyword{ multivariate }
\keyword{ hplot }
