\name{GoodmanKruskalGamma}
\alias{GoodmanKruskalGamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Goodman Kruskal's Gamma
%%  ~~function to do ... ~~
}
\description{Calculate Goodman Kruskal's Gamma statistic, a measure of
association for ordinal factors in a two-way table.\cr
The function has interfaces for a table (matrix) and for single vectors.}
\usage{
GoodmanKruskalGamma(x, y = NULL, conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector or a contingency table. A matrix will be treated as a table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, \dots)} is calculated.
%%     ~~Describe \code{y} here~~
}
  \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence intervals will be calculated.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The estimator of \eqn{\gamma}{gamma} is based only on the number of concordant and discordant pairs of observations. It ignores tied pairs (that is, pairs of observations that have equal values of X or equal values of Y). Gamma is appropriate only when both variables lie on an ordinal scale. \cr
It has the range [-1, 1]. If the two variables are independent, then the estimator of gamma tends to be close to zero.
For \eqn{2 \times 2}{2 x 2} tables, gamma is equivalent to Yule's Q (\code{\link{YuleQ}}). \cr
Gamma is estimated by \deqn{ G = \frac{P-Q}{P+Q}}{G = (P-Q) / (P+Q) } where P equals twice the number of concordances and Q twice the number of discordances.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons,
pp. 57-59.

Brown, M.B., Benedetti, J.K.(1977) Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables, \emph{Journal of the American Statistical Association}, 72, 309-315.

Goodman, L. A., & Kruskal, W. H. (1954) Measures of
association for cross classifications. \emph{Journal of the
American Statistical Association}, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310-364.

%% ~put references to the literature/web site here ~
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{There's another implementation of gamma in \pkg{vcdExtra} \code{\link[vcdExtra]{GKgamma}}\cr
\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{KendallTauA}} (tau-a), \code{\link{KendallTauB}} (tau-b), \code{\link{cor}} (method="kendall") for tau-b, \code{\link{StuartTauC}} (tau-c), \code{\link{SomersDelta}}\cr
\code{\link{Lambda}}, \code{\link{GoodmanKruskalTau}} (tau), \code{\link{UncertCoef}}, \code{\link{MutInf}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(
  c(26,26,23,18, 9),
  c( 6, 7, 9,14,23))
  )

GoodmanKruskalGamma(tab, conf.level=0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
