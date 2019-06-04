\name{ShapiroFranciaTest}
\alias{ShapiroFranciaTest}
\title{Shapiro-Francia Test for Normality}
\description{
  Performs the Shapiro-Francia test for the composite hypothesis of normality.
}
\usage{
ShapiroFranciaTest(x)
}

\arguments{
  \item{x}{a numeric vector of data values, the number of
  which must be between 5 and 5000. Missing values are allowed.}
}
\details{The test statistic of the Shapiro-Francia test is simply the
squared correlation between the ordered sample values and the (approximated)
expected ordered quantiles from the standard normal
distribution. The p-value is computed from the formula given by Royston (1993).
}
\value{
A list of class \code{htest}, containing the following components:
  \item{statistic}{the value of the Shapiro-Francia  statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string \dQuote{Shapiro-Francia normality test}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{Royston, P. (1993): A pocket-calculator algorithm for the
Shapiro-Francia test for non-normality: an application to medicine.
\emph{Statistics in Medicine}, 12, 181--184.

Thode Jr., H.C. (2002): \emph{Testing for  Normality}. Marcel Dekker, New York. (2002, Sec. 2.3.2)}

\author{Juergen Gross <gross@statistik.uni-dortmund.de>}
\note{The Shapiro-Francia test is known to perform well,
see also the comments by Royston (1993). The expected ordered quantiles
from the standard normal distribution are approximated by
\code{qnorm(ppoints(x, a = 3/8))}, being slightly different from the approximation
\code{qnorm(ppoints(x, a = 1/2))} used for the normal quantile-quantile plot by
\code{\link{qqnorm}} for sample sizes greater than 10.}

\seealso{\code{\link{shapiro.test}} for performing the Shapiro-Wilk test for normality.
\code{\link{AndersonDarlingTest}}, \code{\link{CramerVonMisesTest}},
\code{\link{LillieTest}}, \code{\link{PearsonTest}} for performing further tests for normality.
\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
ShapiroFranciaTest(rnorm(100, mean = 5, sd = 3))
ShapiroFranciaTest(runif(100, min = 2, max = 4))
}
\keyword{htest}
