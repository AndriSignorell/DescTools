\name{Lc}
\alias{Lc}
\alias{Lorenz curve}
\alias{Lc.default}
\alias{Lc.formula}
\alias{plot.Lc}
\alias{plot.Lclist}
\alias{lines.Lc}
\alias{predict.Lc}
\title{Lorenz Curve}
\description{
Lc computes the (empirical) ordinary and generalized Lorenz curve of a vector x. Desc calculates some key figures for a Lorenz curve and produces a quick description.}
\usage{

Lc(x, ...)

\method{Lc}{default}(x, n = rep(1, length(x)), na.rm = FALSE, ...)

\method{Lc}{formula}(formula, data, subset, na.action, ...)

\method{plot}{Lc}(x, general = FALSE, lwd = 2, type = "l", xlab = "p", ylab = "L(p)",
     main = "Lorenz curve", las = 1, pch = NA, ...)

\method{plot}{Lclist}(x, col = 1, lwd = 2, lty = 1, main = "Lorenz curve",
     xlab = "p", ylab = "L(p)", ...)

\method{lines}{Lc}(x, general = FALSE, lwd = 2, conf.level = NA, args.cband = NULL, ...)

\method{predict}{Lc}(object, newdata, conf.level=NA, general=FALSE, n=1000, ...)

}
\arguments{
 \item{x}{a vector containing non-negative elements, or a Lc-object for plot and lines.}
 \item{n}{a vector of frequencies, must be same length as \code{x}.}
 \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.}
 \item{general}{logical. If \code{TRUE} the empirical Lorenz curve will be plotted.}
 \item{col}{color of the curve}
 \item{lwd}{the linewidth of the curve}
 \item{lty}{the linetype of the curve}
 \item{type}{type of the plot, default is line (\code{"l"}).}
 \item{xlab, ylab}{label of the x-, resp. y-axis.}
 \item{pch}{the point character (default is \code{NA}, meaning no points will be drawn)}
 \item{main}{main title of the plot.}
 \item{las}{las of the axis.}
%%  \item{p}{a numeric vector with percent points, at which the Lorenz curve will be calculated.
%%     ~~Describe \code{p} here~~
%% }
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{conf.level}{confidence level for the bootstrap confidence interval. Set this to \code{NA}, if no confidence band should be plotted.
  Default is \code{NA}.
%%     ~~Describe \code{type} here~~
}
  \item{args.cband}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}).
%%     ~~Describe \code{type} here~~
}
 \item{object}{object of class inheriting from "Lc"}
 \item{newdata}{an optional vector of percentages p for which to predict. If omitted, the original values of the object are used.}

 \item{\dots}{further argument to be passed to methods.}

}
\details{\code{Lc(x)} computes the empirical ordinary Lorenz curve of \code{x}
  as well as the generalized Lorenz curve (= ordinary Lorenz curve *
  mean(x)). The result can be interpreted like this: \code{p}*100 percent
  have \code{L(p)}*100 percent of \code{x}.

  If \code{n} is changed to anything but the default \code{x} is
  interpreted as a vector of class means and \code{n} as a vector of
  class frequencies: in this case \code{Lc} will compute the minimal
  Lorenz curve (= no inequality within each group).
}
\value{
  A list of class \code{"Lc"} with the following components:
\item{p}{vector of percentages}
\item{L}{vector with values of the ordinary Lorenz curve}
\item{L.general}{vector with values of the generalized Lorenz curve}
\item{x}{the original x values (needed for computing confidence intervals)}
\item{n}{the original n values}
}
\note{ These functions were previously published as \code{Lc()} in the  \pkg{ineq} package and have been
integrated here without logical changes.
}

\references{
Arnold, B. C. (1987) Majorization and the Lorenz Order: A Brief Introduction, \emph{Springer}

Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.

Cowell, F. A. (1995) Measuring Inequality \emph{Harvester Wheatshef: Prentice Hall}.

}

\author{Achim Zeileis <Achim.Zeileis@R-project.org>, extensions Andri Signorell <andri@signorell.net>}


\seealso{The original location \code{\link[ineq]{Lc}()}, \cr
inequality measures \code{\link[ineq]{Gini}()}, \code{\link[ineq]{Atkinson}()}
}

\examples{
priceCarpenter <- d.pizza$price[d.pizza$driver=="Carpenter"]
priceMiller <- d.pizza$price[d.pizza$driver=="Miller"]

# compute the Lorenz curves
Lc.p <- Lc(priceCarpenter, na.rm=TRUE)
Lc.u <- Lc(priceMiller, na.rm=TRUE)
plot(Lc.p)
lines(Lc.u, col=2)

# the picture becomes even clearer with generalized Lorenz curves
plot(Lc.p, general=TRUE)
lines(Lc.u, general=TRUE, col=2)

# inequality measures emphasize these results, e.g. Atkinson's measure
Atkinson(priceCarpenter, na.rm=TRUE)
Atkinson(priceMiller, na.rm=TRUE)


# income distribution of the USA in 1968 (in 10 classes)
# x vector of class means, n vector of class frequencies
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
n <- c(482, 825, 722, 690, 661, 760, 745, 2140, 1911, 1024)

# compute minimal Lorenz curve (= no inequality in each group)
Lc.min <- Lc(x, n=n)
plot(Lc.min)


# input of frequency tables with midpoints of classes
fl <- c(2.5,7.5,15,35,75,150)   # midpoints
n  <- c(25,13,10,5,5,2)	        # frequencies

plot(Lc(fl, n),                 # Lorenz-Curve
     panel.first=grid(10, 10),
     main="Lorenzcurve Farmers",
     xlab="Percent farmers (cumulative)",
     ylab="Percent of area (\%)"
)
# add confidence band
lines(Lc(fl, n), conf.level=0.95,
      args.cband=list(col=SetAlpha(DescToolsOptions("col")[2], 0.3)))

Gini(x=fl, weights=n)

# find specific function values using predict
x <- c(1,1,4)
lx <- Lc(x)
plot(lx)

# get interpolated function value at p=0.55
y0 <- predict(lx, newdata=0.55)
abline(v=0.55, h=y0$L, lty="dotted")

# and for the inverse question use approx
y0 <- approx(x=lx$L, y=lx$p, xout=0.6)
abline(h=0.6, v=y0$y, col="red")

text(x=0.1, y=0.65, label=expression(L^{-1}*(0.6) == 0.8), col="red")
text(x=0.65, y=0.2, label=expression(L(0.55) == 0.275))

# input of frequency tables with midpoints of classes
fl <- c(2.5,7.5,15,35,75,150)     # midpoints
n  <- c(25,13,10,5,5,2)           # frequencies

# the formula interface for Lc
lst <- Lc(count ~ cut(price, breaks=5), data=d.pizza)

plot(lst, col=1:length(lst), panel.first=grid(), lwd=2)
legend(x="topleft", legend=names(lst), fill=1:length(lst))

# Describe with Desc-function
lx <- Lc(fl, n)
Desc(lx)

}
\keyword{univar}


