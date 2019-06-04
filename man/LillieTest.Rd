\name{LillieTest}
\alias{LillieTest}
\title{Lilliefors (Kolmogorov-Smirnov) Test for Normality}
\description{
  Performs the Lilliefors (Kolmogorov-Smirnov) test for the composite hypothesis of normality,
  see e.g. Thode (2002, Sec. 5.1.1).
}
\usage{
LillieTest(x)
}

\arguments{
  \item{x}{a numeric vector of data values, the number of
  which must be greater than 4. Missing values are allowed.}
}
\details{The Lilliefors (Kolmogorov-Smirnov) test is an EDF omnibus test for the composite
hypothesis of normality. The test statistic is the maximal absolute difference
between empirical and
hypothetical cumulative distribution function. It may be computed as
\eqn{D=\max\{D^{+}, D^{-}\}} with
\deqn{
D^{+} = \max_{i=1,\ldots, n}\{i/n - p_{(i)}\},
D^{-} = \max_{i=1,\ldots, n}\{p_{(i)} - (i-1)/n\},
}
where \eqn{p_{(i)} = \Phi([x_{(i)} - \overline{x}]/s)}. Here,
\eqn{\Phi} is the cumulative distribution function
of the standard normal distribution, and \eqn{\overline{x}} and \eqn{s}
are mean and standard deviation of the data values.
The p-value is computed from the Dallal-Wilkinson (1986) formula, which is claimed to
be only reliable when the p-value is smaller than 0.1. If the Dallal-Wilkinson
p-value turns out to be greater than 0.1, then the p-value is computed from the distribution of
the modified statistic \eqn{Z=D (\sqrt{n}-0.01+0.85/\sqrt{n})}, see Stephens (1974),
the actual p-value formula being obtained by a simulation and approximation process.}

\value{
A list of class \code{htest}, containing the following components:
  \item{statistic}{the value of the Lilliefors (Kolomogorv-Smirnov) statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string \dQuote{Lilliefors (Kolmogorov-Smirnov) normality test}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
Dallal, G.E. and Wilkinson, L. (1986)
An analytic approximation to the distribution of Lilliefors' test for normality.
\emph{The American Statistician}, 40, 294--296.


Stephens, M.A. (1974) EDF statistics for goodness of fit and some comparisons.
\emph{Journal of the American Statistical Association}, 69, 730--737.

Thode Jr., H.C. (2002) \emph{Testing for  Normality} Marcel Dekker, New York.
}
\author{Juergen Gross <gross@statistik.uni-dortmund.de>}
\note{The Lilliefors (Kolomorov-Smirnov) test is the most famous EDF omnibus test for normality.
Compared to the Anderson-Darling test and the Cramer-von Mises test it is known to perform worse.
Although the test statistic obtained from \code{LillieTest(x)} is the same as that obtained from
\code{ks.test(x, "pnorm", mean(x), sd(x))}, it is not correct to use the p-value from the latter
for the composite hypothesis of normality (mean and variance unknown),
since the distribution of the test statistic is different when the parameters are estimated.

The function call \code{LillieTest(x)} essentially produces
the same result as the S-PLUS function call \code{ks.gof(x)}
with the distinction that the p-value is not set to 0.5 when
the Dallal-Wilkinson approximation yields a p-value greater than 0.1. (Actually,
the alternative p-value approximation is provided for the complete range of test statistic values,
but is only used when the Dallal-Wilkinson approximation fails.)}

\seealso{\code{\link{shapiro.test}} for performing the Shapiro-Wilk test for normality.
\code{\link{AndersonDarlingTest}}, \code{\link{CramerVonMisesTest}},
\code{\link{PearsonTest}}, \code{\link{ShapiroFranciaTest}} for performing further tests for normality.
\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
LillieTest(rnorm(100, mean = 5, sd = 3))
LillieTest(runif(100, min = 2, max = 4))
}
\keyword{htest}
