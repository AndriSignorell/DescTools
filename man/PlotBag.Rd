\name{PlotBag}
\alias{PlotBag}
\alias{PlotBagPairs}
\alias{compute.bagplot}
\alias{plot.bagplot}


\title{Bivariate Boxplot }
\description{
\code{PlotBag()} creates a twodimensional boxplot called "bagplot" based on two numerical variables x and y. \code{plot.PlotBag()} is the plotting routine for a bagplot object. \code{compute.PlotBag()} contains the computation logic the object.

}
\usage{
PlotBag(x, y, factor = 3, na.rm = FALSE, approx.limit = 300,
        show.outlier = TRUE, show.whiskers = TRUE,
        show.looppoints = TRUE, show.bagpoints = TRUE,
        show.loophull = TRUE, show.baghull = TRUE,
        create.plot = TRUE, add = FALSE, pch = 16, cex = 0.4,
        dkmethod = 2, precision = 1, verbose = FALSE,
        debug.plots = "no",   col.loophull = "#aaccff",
        col.looppoints = "#3355ff", col.baghull = "#7799ff",
        col.bagpoints = "#000088", transparency = FALSE, ...
)
PlotBagPairs(dm, trim = 0.0, main, numeric.only = TRUE,
             factor = 3, approx.limit = 300, pch = 16,
             cex = 0.8, precision = 1, col.loophull = "#aaccff",
             col.looppoints = "#3355ff", col.baghull = "#7799ff",
             col.bagpoints = "#000088", ...)

compute.bagplot(x, y, factor = 3, na.rm = FALSE, approx.limit = 300,
                dkmethod = 2, precision = 1, verbose = FALSE, debug.plots = "no" )

\method{plot}{bagplot}(x, show.outlier = TRUE, show.whiskers = TRUE,
     show.looppoints = TRUE, show.bagpoints = TRUE,
     show.loophull = TRUE, show.baghull = TRUE, add = FALSE,
     pch = 16, cex = .4, verbose = FALSE, col.loophull = "#aaccff",
     col.looppoints = "#3355ff", col.baghull = "#7799ff",
     col.bagpoints = "#000088", transparency = FALSE,...)

}
\arguments{
  \item{x}{ x values of a data set;
     in \code{PlotBag}: an object of class \code{PlotBag}
     computed by \code{compute.PlotBag} }
  \item{y}{ y values of the data set }
  \item{factor}{ factor defining the loop }
  \item{na.rm}{ if \code{TRUE} 'NA' values are removed otherwise exchanged by median}
  \item{approx.limit}{ if the number of data points exceeds
          \code{approx.limit} a sample is used to compute
          some of the quantities; default: 300 }
  \item{show.outlier}{ if \code{TRUE} outlier are shown }
  \item{show.whiskers}{ if \code{TRUE} whiskers are shown }
  \item{show.looppoints}{ if \code{TRUE} loop points are plottet }
  \item{show.bagpoints}{ if \code{TRUE} bag points are plottet }
  \item{show.loophull}{ if \code{TRUE} the loop is plotted }
  \item{show.baghull}{ if \code{TRUE} the bag is plotted }
  \item{create.plot}{ if \code{FALSE} no plot is created }
  \item{add}{ if \code{TRUE} the bagplot is added to an existing plot }
  \item{pch}{ sets the plotting character }
  \item{cex}{ sets characters size}
  \item{dkmethod}{ 1 or 2, there are two method of
     approximating the bag, method 1 is very rough (only based on observations }
  \item{precision}{ precision of approximation, default: 1 }
  \item{verbose}{ automatic commenting of calculations }
  \item{debug.plots}{ if \code{TRUE} additional plots describing
                      intermediate results are constructed }
  \item{col.loophull}{ color of loop hull }
  \item{col.looppoints}{ color of the points of the loop }
  \item{col.baghull}{ color of bag hull }
  \item{col.bagpoints}{ color of the points of the bag }
  \item{transparency}{ see section details }
  \item{dm}{ x }
  \item{trim}{ x }
  \item{main}{ x }
  \item{numeric.only}{ x }
  \item{\dots}{ additional graphical parameters }
}
\details{
A bagplot is a bivariate generalization of the well known
boxplot. It has been proposed by Rousseeuw, Ruts, and Tukey.
In the bivariate case the box of the boxplot changes to a
convex polygon, the bag of bagplot. In the bag are 50 percent
of all points. The fence separates points within the fence from
points outside. It is computed by increasing the
the bag. The loop is defined as the convex hull containing
all points inside the fence.
If all points are on a straight line you get a classical
boxplot.
\code{PlotBag()} plots bagplots that are very similar
to the one described in Rousseeuw et al.
Remarks:
The two dimensional median is approximated.
For large data sets the error will be very small.
On the other hand it is not very wise to make a (graphical)
summary of e.g. 10 bivariate data points.

In case you want to plot multiple (overlapping) bagplots,
you may want plots that are semi-transparent. For this
you can use the \code{transparency} flag.
If \code{transparency==TRUE} the alpha layer is set to '99' (hex).
This causes the bagplots to appear semi-transparent,
but ONLY if the output device is PDF and opened using:
\code{pdf(file="filename.pdf", version="1.4")}.
For this reason, the default is \code{transparency==FALSE}.
This feature as well as the arguments
to specify different colors has been proposed by Wouter Meuleman.
}
\value{
  \code{compute.bagplot} returns an object of class
  \code{bagplot} that could be plotted by
  \code{plot.bagplot()}.
  An object of the bagplot class is a list with the following
  elements: \code{center} is a two dimensional vector with
  the coordinates of the center. \code{hull.center} is a
  two column matrix, the rows are the coordinates of the
  corners of the center region. \code{hull.bag} and
  \code{hull.loop} contain the coordinates of the hull of the bag
  and the hull of the loop. \code{pxy.bag} shows you the
  coordinates of the points of the bag. \code{pxy.outer} is
  the two column matrix of the points that are within the
  fence. \code{pxy.outlier} represent the outliers. The vector
  \code{hdepths} shows the depths of data points. \code{is.one.dim}
  is \code{TRUE} if the data set is (nearly) one dimensional.
  The dimensionality is decided by analysing the result of \code{prcomp}
  which is stored in the element \code{prdata}. \code{xy} shows you
  the data that are used for the bagplot. In the case of very large
  data sets subsets of the data are used for constructing the
  bagplot. A data set is very large if there are more data points
  than \code{approx.limit}. \code{xydata} are the input data structured
  in a two column matrix.
}
\references{ P. J. Rousseeuw, I. Ruts, J. W. Tukey (1999):
    The bagplot: a bivariate boxplot, \emph{The American
    Statistician}, vol. 53, no. 4, 382--387 }

\author{Hans Peter Wolf <pwolf@wiwi.uni-bielefeld.de> }

\note{
Version of bagplot: 10/2012 }
\seealso{ \code{\link[graphics]{boxplot}} }

\examples{
# example: 100 random points and one outlier
dat <- cbind(rnorm(100) + 100, rnorm(100) + 300)
dat <- rbind(dat, c(105,295))

PlotBag(dat,factor=2.5,create.plot=TRUE,approx.limit=300,
   show.outlier=TRUE,show.looppoints=TRUE,
   show.bagpoints=TRUE,dkmethod=2,
   show.whiskers=TRUE,show.loophull=TRUE,
   show.baghull=TRUE,verbose=FALSE)

# example of Rousseeuw et al., see R-package rpart
cardata <- structure(as.integer( c(2560,2345,1845,2260,2440,
 2285, 2275, 2350, 2295, 1900, 2390, 2075, 2330, 3320, 2885,
 3310, 2695, 2170, 2710, 2775, 2840, 2485, 2670, 2640, 2655,
 3065, 2750, 2920, 2780, 2745, 3110, 2920, 2645, 2575, 2935,
 2920, 2985, 3265, 2880, 2975, 3450, 3145, 3190, 3610, 2885,
 3480, 3200, 2765, 3220, 3480, 3325, 3855, 3850, 3195, 3735,
 3665, 3735, 3415, 3185, 3690, 97, 114, 81, 91, 113, 97, 97,
 98, 109, 73, 97, 89, 109, 305, 153, 302, 133, 97, 125, 146,
 107, 109, 121, 151, 133, 181, 141, 132, 133, 122, 181, 146,
 151, 116, 135, 122, 141, 163, 151, 153, 202, 180, 182, 232,
 143, 180, 180, 151, 189, 180, 231, 305, 302, 151, 202, 182,
 181, 143, 146, 146)), .Dim = as.integer(c(60, 2)),
 .Dimnames = list(NULL, c("Weight", "Disp.")))

PlotBag(cardata,factor=3,show.baghull=TRUE,
  show.loophull=TRUE,precision=1, dkmethod=2)

title("car data Chambers/Hastie 1992")

# points of y=x*x
PlotBag(x=1:30,y=(1:30)^2,verbose=FALSE,dkmethod=2)

# one dimensional subspace
PlotBag(x=1:50,y=1:50)

# pairwise bagplots
par(las=1)
PlotBagPairs(swiss[, 1:2],
             main="Swiss Fertility and Socioeconomic Indicators (1888) Data")
}
\keyword{ misc }
\keyword{ hplot }
