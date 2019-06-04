\name{JarqueBeraTest}
\alias{JarqueBeraTest}
\title{(Robust) Jarque Bera Test}
\description{
This function performs the Jarque-Bera tests of normality either the robust or the classical way.
}
\usage{
JarqueBeraTest(x, robust = TRUE, method = c("chisq", "mc"),
               N = 0, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{robust}{defines, whether the robust version should be used.
  Default is \code{TRUE}.}
  \item{method}{a character string out of \code{chisq} or \code{mc}, specifying how the critical
                 values should be obtained. Default is approximated by the
                 chisq-distribution or empirically via Monte Carlo.}
  \item{N}{number of Monte Carlo simulations for the empirical critical values}
  \item{na.rm}{defines if \code{NAs} should be omitted. Default is \code{FALSE}.}
}
\details{
The test is based on a joint statistic using skewness and kurtosis
coefficients. The robust Jarque-Bera (RJB) version of utilizes
the robust standard deviation (namely the mean absolute deviation
from the median, as provided e. g. by \code{\link{MeanAD}(x, FUN=median)}) to estimate sample kurtosis and skewness. For more details see Gel and Gastwirth (2006).
\cr
Setting \code{robust} to \code{FALSE} will perform the original Jarque-Bera test (see
Jarque, C. and Bera, A (1980)).
}
\value{
A list with class \code{htest} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\note{This function is melted from the \code{jarque.bera.test} (in \code{tseries} package) and the \code{rjb.test} from the package \code{lawstat}.}
\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R. and Gastwirth, J. L. (2008) \emph{A robust modification of
the Jarque-Bera test of normality}, Economics Letters 99, 30-32.\cr

Jarque, C. and Bera, A. (1980) \emph{Efficient tests for
normality, homoscedasticity and serial independence of regression
residuals}, Economics Letters 6, 255-259.
}
\author{W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{Alternative tests for normality as
\code{\link{shapiro.test}},
\code{\link{AndersonDarlingTest}}, \code{\link{CramerVonMisesTest}}, \code{\link{LillieTest}}, \code{\link{PearsonTest}}, \code{\link{ShapiroFranciaTest}}

\code{\link{qqnorm}}, \code{\link{qqline}} for producing a normal quantile-quantile plot
}
\examples{
x <- rnorm(100)    # null hypothesis
JarqueBeraTest(x)

x <- runif(100)    # alternative hypothesis
JarqueBeraTest(x, robust=TRUE)
}
\keyword{htest}
