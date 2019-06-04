\name{BartelsRankTest}
\alias{BartelsRankTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bartels Rank Test of Randomness
}
\description{
Performs the Bartels rank test of randomness, which tests if a sample is sampled randomly from an underlying population. Data must at least be measured on an ordinal scale.
}
\usage{
BartelsRankTest(x, alternative = c("two.sided", "trend", "oscillation"),
                method = c("normal", "beta", "auto"))
}
% alternative = c("two.sided", "less", "greater")
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector containing the observations}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "\code{two.sided}" (default), "\code{trend}" or "\code{oscillation}". }
  \item{method}{a character string specifying the method used to compute the p-value. Must be one of \code{normal} (default), \code{beta} or \code{auto}. }
}
\details{
The RVN test statistic is
\deqn{RVN=\frac{\sum_{i=1}^{n-1}(R_i-R_{i+1})^2}{\sum_{i=1}^{n}\left(R_i-(n+1)/2\right)^2}}{RVN=\sum(R_i-R_{i+1})^2 / \sum(R_i-(n+1)/2)^2}
where \eqn{R_i=rank(X_i), i=1,\dots, n}{R_i=rank(X_i), i=1,...,n}. It is known that \eqn{(RVN-2)/\sigma} is asymptotically standard normal, where \eqn{\sigma^2=\frac{4(n-2)(5n^2-2n-9)}{5n(n+1)(n-1)^2}}{\sigma^2=[4(n-2)(5n^2-2n-9)]/[5n(n+1)(n-1)^2]}.

By using the alternative "\code{trend}" the null hypothesis of randomness is tested against a trend. By using the alternative "\code{oscillation}" the null hypothesis of randomness is tested against a systematic oscillation.

Missing values are silently removed.

Bartels test is a rank version of von Neumann's test.
}
\value{
A list with class "htest" containing the components:
\item{statistic}{the value of the normalized statistic test.}
\item{parameter, n}{the size of the data, after the remotion of consecutive duplicate values.}
\item{p.value}{the p-value of the test.}
\item{alternative}{a character string describing the alternative hypothesis.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
\item{rvn}{the value of the RVN statistic (not show on screen).}
\item{nm}{the value of the NM statistic, the numerator of RVN (not show on screen).}
\item{mu}{the mean value of the RVN statistic (not show on screen).}
\item{var}{the variance of the RVN statistic (not show on screen).}
}
\references{
Bartels, R. (1982) The Rank Version of von Neumann's Ratio Test for Randomness, \emph{Journal of the American Statistical Association}, \bold{77} (377), 40-46.

Gibbons, J.D. and Chakraborti, S. (2003) \emph{Nonparametric Statistical Inference}, 4th ed. (pp. 97-98).
URL: \url{http://books.google.pt/books?id=dPhtioXwI9cC&lpg=PA97&ots=ZGaQCmuEUq}

von Neumann, J. (1941) Distribution of the ratio of the mean square successive difference to the variance.
\emph{Annals of Mathematical Statistics} \bold{12}, 367-395.
}
\author{Frederico Caeiro <fac@fct.unl.pt>}

\seealso{
\code{\link[randtests]{rank.test}}, \code{\link{RunsTest}}
}
\examples{
## Example 5.1 in Gibbons and Chakraborti (2003), p.98.
## Annual data on total number of tourists to the United States for 1970-1982.

years <- 1970:1982
tourists <- c(12362, 12739, 13057, 13955, 14123,  15698, 17523, 18610, 19842,
      20310, 22500, 23080, 21916)
plot(years, tourists, pch=20)

BartelsRankTest(tourists, alternative="trend", method="beta")

#  Bartels Ratio Test
#
# data:  tourists
# statistic = -3.6453, n = 13, p-value = 1.21e-08
# alternative hypothesis: trend


## Example in Bartels (1982).
## Changes in stock levels for 1968-1969 to 1977-1978 (in $A million), deflated by the
## Australian gross domestic product (GDP) price index (base 1966-1967).
x <- c(528, 348, 264, -20, - 167, 575, 410, -4, 430, - 122)

BartelsRankTest(x, method="beta")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ bartels rank test }
\keyword{htest }% __ONLY ONE__ keyword per line
