\name{TukeyBiweight}
\alias{TukeyBiweight}
\title{ Calculate Tukey's Biweight Robust Mean }
\description{
  This calculates a robust average that is unaffected by outliers.
}
\usage{
TukeyBiweight(x, const = 9, na.rm = FALSE,
              conf.level = NA, ci.type = "bca", R=1000, ...)
}
\arguments{
  \item{x}{ a \code{numeric} vector }
  \item{const}{ a constant. \code{\var{const}} is preassigned a value of 9
    according to the Cook reference below but other values are
    possible. }
  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to FALSE.
}
   \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated.
%%     ~~Describe \code{conf.level} here~~
}
  \item{ci.type}{The type of confidence interval required. The value should be any subset
 of the values \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"} or simply \code{"all"}
 which will compute all four types of intervals.
}
\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling,
 some resamples may use one set of weights and others use a different set of weights. In this case \code{R} would be a vector
 of integers where each component gives the number of resamples from each of the rows of weights.
}
\item{\dots}{the dots are passed to the function \code{\link{boot}}, when confidence intervalls are calculated.
}

}
\details{
  This is a one step computation that follows the Affy whitepaper below,
  see page 22. \code{\var{const}} determines the point at which
  outliers are given a weight of 0 and therefore do not contribute to
  the calculation of the mean.  \code{\var{const} = 9} sets values roughly
  +/-6 standard deviations to 0. \code{\var{const} = 6} is also used in
  tree-ring chronology development. Cook and Kairiukstis (1990) have
  further details.

  An exact summation algorithm (Shewchuk 1997) is used. When some
  assumptions about the rounding of floating point numbers and
  conservative compiler optimizations hold, summation error is
  completely avoided.  Whether the assumptions hold depends on the
  platform, i.e. compiler and \acronym{CPU}.
}
\value{
  A \code{numeric} mean.
}
\references{

  Statistical Algorithms Description Document, 2002, Affymetrix.

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Mosteller, F. and Tukey, J. W. (1977) \emph{Data Analysis and
  Regression: a second course in statistics}.  Addison-Wesley.
  \acronym{ISBN-13}: 978-0201048544.

  Shewchuk, J. R. (1997) Adaptive Precision Floating-Point Arithmetic
  and Fast Robust Geometric Predicates.  \emph{Discrete and
  Computational Geometry}, 18(3):305-363. Springer.

}
\author{ Mikko Korpela <mikko.korpela@aalto.fi> }

\seealso{ \code{\link{HuberM}}, \code{\link{Range}}, \code{\link{RobScale}} }
\examples{
TukeyBiweight(rnorm(100))
}
\keyword{ robust }
\keyword{ univar }
