% File src/library/stats/man/median.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Median}
\title{(Weighted) Median Value}
\alias{Median}
\alias{Median.Freq}
\alias{Median.factor}
\alias{Median.default}

\usage{
Median(x, \dots)

\method{Median}{factor}(x, na.rm = FALSE, \dots)

\method{Median}{Freq}(x, breaks, \dots)

\method{Median}{default}(x, weights = NULL, na.rm = FALSE, \dots)
}
\arguments{
  \item{x}{an object for which a method has been defined, or a
    numeric vector containing the values whose median is to be computed.}

\item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }
  \item{breaks}{breaks for calculating the mean for classified data as composed by \code{\link{Freq}}.}

  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}

  \item{\dots}{further arguments passed to or from other methods.}

}
\description{
  Compute the sample median. The function basically wraps the function \code{\link{Quantile}()}, which offers the option to define weights.\cr
  For grouped data the median can be estimated by linear interpolation within the class containing the median, which is implemented in the interface for \code{Freq}-objects.
}
\details{
  This is a generic function for which methods can be written.  However,
  the default method makes use of \code{is.na}, \code{sort} and
  \code{mean} from package \pkg{base} all of which are generic, and so
  the default method will work for most classes
  (e.g., \code{"\link{Date}"}) for which a median is a reasonable
  concept.

  Calculating the median for ordered factors is not implemented in standard R, as
it's not well defined (it is not clear what to do if the median sits between two levels in factors of even length).
This function returns the high median and prints a warning if the low median would be different (which is
supposed to be a rare event).
There's a vivid discussion between experts going on whether this should be defined or not.
We'll wait for definitive results and enjoy the function's comfort so far...

Note that there are alternative approaches for calculating weighted median (e.g. \code{matrixstats::weightedMedian}).
}
\value{
  The default method returns a length-one object of the same type as
  \code{x}, except when \code{x} is integer of even length, when the
  result will be double.

  If there are no values or if \code{na.rm = FALSE} and there are \code{NA}
  values the result is \code{NA} of the same type as \code{x} (or more
  generally the result of \code{x[FALSE][NA]}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{\code{\link{quantile}} for general quantiles.
\url{https://stat.ethz.ch/pipermail/r-help/2003-November/042684.html}

\url{https://stackoverflow.com/questions/7925102/idiomatic-method-of-finding-the-median-of-an-ordinal}
}
\examples{
Median(1:4)                # = 2.5 [even number]
Median(c(1:3, 100, 1000))  # = 3 [odd, robust]

# Approximation for classified data
breaks <- seq(10,70, 10)
Median(
  Freq(cut(d.pizza$temperature, breaks=breaks)),
  breaks=breaks)

# compared to
Median(d.pizza$temperature)

# starting from a classified table
# from     to  income
#    0   4000      20
# 4000   6000      42
# 6000   8000      31
# 8000  10000      12

# Freq(as.table(c(20,42,31,12)))
#    level  freq   perc  cumfreq  cumperc
# 1      A    20  19.0\%       20    19.0\%
# 2      B    42  40.0\%       62    59.0\%
# 3      C    31  29.5\%       93    88.6\%
# 4      D    12  11.4\%      105   100.0\%

Median(Freq(as.table(c(20,42,31,12))), breaks=c(0,4000,6000,8000,10000))

# use weights
x <- sample(20, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))

(m1 <- Median(z, weights=w))
(m2 <- Median(x))
stopifnot(identical(m1, m2))
}
\keyword{univar}
\keyword{robust}
