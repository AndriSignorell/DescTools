\name{RunsTest}
\alias{RunsTest}
\alias{RunsTest.formula}
\alias{RunsTest.default}
\title{Runs Test for Randomness}
\description{
Performs a test whether the elements of \code{x} are serially independent - say, whether
they occur in a random order - by counting how many runs there are above and below a threshold.
If \code{y} is supplied a two sample Wald-Wolfowitz-Test testing the equality of two distributions against general alternatives will be computed.
}
\usage{
RunsTest(x, ...)

\method{RunsTest}{default}(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         exact = NULL, correct = TRUE, na.rm = FALSE, ...)

\method{RunsTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{a dichotomous vector of data values or a (non-empty) numeric vector of data values.}
  \item{y}{an optional (non-empty) numeric vector of data values.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"less"} or \code{"greater"}. }
  \item{exact}{a logical indicating whether an exact p-value should be computed. By default exact values will be calculated for small vectors with a total length <= 30 and the normal approximation for longer ones.}
\item{correct}{a logical indicating whether to apply continuity correction when computing the test statistic. Default is \code{TRUE}. Ignored if \code{exact} is set to \code{TRUE}. See details.}
  \item{na.rm}{defines if \code{NA}s should be omitted. Default is \code{FALSE}. }
  \item{\dots}{further arguments to be passed to or from methods.}

}

% http://www.sussex.ac.uk/its/pdfs/SPSS_Exact_Tests_21.pdf:
% For the Wald-Wolfowitz runs test, only twosided
% p values exist, and they are always computed from the left tail of the null
% distribution of the test statistic.
% What does that mean?????

\details{
The runs test for randomness is used to test the hypothesis
that a series of numbers is random. The 2-sample test is known as the Wald-Wolfowitz test. \cr

For a categorical variable, the number of runs correspond to the number of times the category changes, that is,
where \eqn{x_{i}}{x_i} belongs to one category and  \eqn{x_{i+1}}{x_(i+1)}  belongs to the other. The number of runs is the number of sign changes plus one.\cr

For a numeric variable x containing more than two values, a run is a set of sequential values that are either all above or below a specified cutpoint, typically the median. This is not necessarily the best choice. If another threshold should be used use a code like: \code{RunsTest(x > mean(x))}.

The exact distribution of runs and the p-value based on it are described in the manual of SPSS "Exact tests"
\url{http://www.sussex.ac.uk/its/pdfs/SPSS_Exact_Tests_21.pdf}.

The normal approximation of the runs test is calculated with the expected number of runs under the null
\deqn{\mu_r=\frac{2 n_0 n_1}{n_0 + n_1} + 1}
and its variance
\deqn{\sigma_r^2 = \frac{2 n_0 n_1 (2 n_0 n_1 - n_0 - n_1) }{(n_0 + n_1)^2 \cdot (n_0 + n_1 - 1)}}
as
\deqn{\hat{z}=\frac{r - \mu_r + c}{\sigma_r}}
where \eqn{n_0, n_1} the number of values below/above the threshold and \eqn{r} the number of runs.

Setting the continuity correction \code{correct = TRUE} will yield the normal approximation as SAS (and SPSS if n < 50) does it, see \url{http://support.sas.com/kb/33/092.html}.
The c is set to \eqn{c = 0.5} if \eqn{r < \frac{2 n_0 n_1}{n_0 + n_1} + 1}
and to \eqn{c = -0.5} if \eqn{r > \frac{2 n_0 n_1}{n_0 + n_1} + 1}.

\bold{Wald-Wolfowitz with Ties.}      Ideally there should be no ties in the data used for the Wald-Wolfowitz test. In practice there is no problem with ties within a group, but if ties occur between members of the different groups then there is no unique sequence of observations. For example the data sets A: 10,14,17,19,34 and B: 12,13,17,19,22 can give four possible sequences, with two possible values for r (7 or 9). The "solution" to this is to list every possible combination, and calculate the test statistic for each one. If all test statistics are significant at the chosen level, then one can reject the null hypothesis. If only some are significant, then Siegel (1956) suggests that the average of the P-values is taken.
Help for finding all permutations of ties can be found at:  \url{https://stackoverflow.com/questions/47565066/all-possible-permutations-in-factor-variable-when-ties-exist-in-r}

However this solutions seems quite coarse and in general, the test should not be used if there are more than one or two ties. We have better tests to distinguish between two samples!
}

\value{
A list with the following components.
  \item{statistic}{z, the value of the standardized runs statistic, if not exact p-values are computed.}
  \item{parameter}{the number of runs, the total number of zeros (m) and ones (n)}
  \item{p.value}{the p-value for the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
}
\references{
Wackerly, D., Mendenhall, W. Scheaffer, R. L. (1986) \emph{Mathematical Statistics with Applications}, 3rd Ed., Duxbury Press, CA.

Wald, A. and Wolfowitz, J. (1940): On a test whether two samples are from the same population,  \emph{Ann. Math Statist}. 11, 147-162.

Siegel, S. (1956) \emph{Nonparametric Statistics for the Behavioural Sciences}, McGraw-Hill Kogakusha, Tokyo.

}

\seealso{
  Run Length Encoding \code{\link{rle}}
}

\author{
  Andri Signorell <andri@signorell.net>, exact p-values by Detlew Labes <detlewlabes@gmx.de>
}

\examples{
# x will be coerced to a dichotomous variable
x <- c("S","S", "T", "S", "T","T","T", "S", "T")
RunsTest(x)


x <- c(13, 3, 14, 14, 1, 14, 3, 8, 14, 17, 9, 14, 13, 2, 16, 1, 3, 12, 13, 14)
RunsTest(x)
# this will be treated as
RunsTest(x > median(x))

plot( (x < median(x)) - 0.5, type="s", ylim=c(-1,1) )
abline(h=0)

set.seed(123)
x <- sample(0:1, size=100, replace=TRUE)
RunsTest(x)
# As you would expect of values from a random number generator, the test fails to reject
# the null hypothesis that the data are random.


# SPSS example
x <- c(31,23,36,43,51,44,12,26,43,75,2,3,15,18,78,24,13,27,86,61,13,7,6,8)
RunsTest(x, exact=TRUE)       # exact probability
RunsTest(x, exact=FALSE)      # normal approximation

# SPSS example small dataset
x <- c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1)
RunsTest(x)
RunsTest(x, exact=FALSE)

# if y is not NULL, the Wald-Wolfowitz-Test will be performed
A <- c(35,44,39,50,48,29,60,75,49,66)
B <- c(17,23,13,24,33,21,18,16,32)

RunsTest(A, B, exact=TRUE)
RunsTest(A, B, exact=FALSE)
}
\keyword{htest}

