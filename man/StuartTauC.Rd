\name{StuartTauC}
\alias{StuartTauC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stuart \eqn{\tau_{c}}{Tau-c}
%%  ~~function to do ... ~~
}
\description{Calculate Stuart's \eqn{\tau_{c}}{tau-c} statistic, a measure of
association for ordinal factors in a two-way table.\cr
The function has interfaces for a table (matrix) and for single vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StuartTauC(x, y = NULL, conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector or a table. A matrix will be treated as table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, \dots)} is calculated.
%%     ~~Describe \code{y} here~~
}
  \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Stuart's \eqn{\tau_{c}}{tau-c} makes an adjustment for table size in addition to a correction for ties. \eqn{\tau_{c}}{Tau-c} is
appropriate only when both variables lie on an ordinal scale. \cr
It is estimated by \cr
\deqn{ \tau_{c} = \frac{m \cdot(P-Q)}{n^2 \cdot (m-1)}}{tau_c = m / n^2 * (P-Q) / (m-1)}
where P equals twice the number of concordances and Q twice the number of discordances, n is the total amount of observations and m = min(R, C). The range of \eqn{\tau_{c}}{tau-c} is [-1, 1]. \cr
See \url{http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf}, pp. 1739 for the estimation of the asymptotic variance.

%%  ~~ If necessary, more details than the description above ~~
}
\value{a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons,
pp. 57--59.

Brown, M.B., Benedetti, J.K.(1977) Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables, \emph{Journal of the American Statistical Association}, 72, 309-315.

Goodman, L. A., & Kruskal, W. H. (1954) Measures of
association for cross classifications. \emph{Journal of the
American Statistical Association}, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310-364.
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{GoodmanKruskalGamma}}, \code{\link{KendallTauA}} (\eqn{\tau_{a}}{tau-a}), \code{\link{cor}} (method="kendall") for \eqn{\tau_{b}}{tau-b}, \code{\link{SomersDelta}}\cr
\code{\link{Lambda}}, \code{\link{GoodmanKruskalTau}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}
}

\examples{
# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

StuartTauC(tab, conf.level=0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
