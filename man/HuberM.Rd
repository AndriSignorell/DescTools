\name{HuberM}
\alias{HuberM}
\title{Safe (generalized) Huber M-Estimator of Location}
\concept{robust location}
\description{
  (Generalized) Huber M-estimator of location with MAD scale, being
  sensible also when the scale is zero where \code{\link[MASS]{huber}()}
  returns an error.
}
\usage{
HuberM(x, k = 1.345, mu = median(x), s = mad(x, center = mu), 
       na.rm = FALSE, conf.level = NA, ci.type = c("wald", "boot"), ...)
}
\arguments{
  \item{x}{numeric vector.}
  \item{k}{positive factor; the algorithm winsorizes at \code{k}
    standard deviations.}
  \item{mu}{initial location estimator.}
  \item{s}{scale estimator held constant through the iterations.}
  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to FALSE.}
 \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated. }
 \item{ci.type}{The type of confidence interval required. The value should be any subset 
 of the values \code{"wald"}, \code{"boot"}. 
}
\item{\dots}{ the dots are passed to the function \code{\link{boot}}, when confidence intervalls are calculated.}
}

\details{
   The standard error is computed using the \eqn{\tau} correction factor but no finite sample correction.\cr The original function is not exported, but can be accessed as \code{DescTools::.huberM}.
  % and as if \code{s} was not estimated from the data.
}

\value{
If \code{conf.level} is set to \code{NA} then the result will be  
\item{a}{ single numeric value} and 
if a \code{conf.level} is provided, a named numeric vector with 3 elements:
  \item{huberm}{the estimate for location}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\author{Martin Maechler, building on the MASS code mentioned.\cr
Andri Signorell <andri@signorell.net> (confidence intervals and interface)}
\references{
  Huber, P. J. (1981)
  \emph{Robust Statistics.}
  Wiley.
}
\seealso{
  \code{\link[MASS]{hubers}} (and \code{huber}) in package \pkg{MASS};
  \code{\link{mad}}.
}
\examples{
HuberM(c(1:9, 1000))
mad   (c(1:9, 1000))

set.seed(7)
x <- c(round(rnorm(1000), 1), round(rnorm(50, m=10, sd = 10)))
HuberM(x, conf.level=0.95)


\dontrun{

# scale zero
HuberM(rep(9, 100))
mad   (rep(9, 100))

# bootstrap confidence intervals
HuberM(x, conf.level=0.95, ci.type="boot")
}
 
}
\keyword{univar}
\keyword{robust}
