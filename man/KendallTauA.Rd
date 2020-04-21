\name{KendallTauA}
\alias{KendallTauA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kendall's \eqn{\tau_{a}}{Tau-a}
%%  ~~function to do ... ~~
}
\description{Calculate Kendall's tau-a statistic, a measure of
association for ordinal factors in a two-way table.\cr
The function has interfaces for a table (matrix) and for single vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
KendallTauA(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector or a table. A matrix will be treated as table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, \dots)} is calculated.
%%     ~~Describe \code{y} here~~
}
 \item{direction}{direction of the calculation. Can be \code{"row"} (default) or \code{"column"}, where
  \code{"row"} calculates Kendall's tau-a (R|C) ("column dependent").
%%     ~~Describe \code{x} here~~
}
\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Kendall's tau coefficient (sometimes called "Kendall rank correlation coefficient"), is a statistic used to measure the association between two measured quantities. It is a measure of rank correlation: the similarity of the orderings of the data when ranked by each of the quantities.
\cr
Kendall's tau-a is computed as \deqn{ \tau_a(C|R) = \frac{P-Q}{\frac{1}{2} \cdot n \cdot (n-1)}}{tau_a = 2 * (P-Q) / (n * (n-1))}
where P equals twice the number of concordances and Q twice the number of discordances. Its range is [-1, 1].\cr
(Note that Kendall tau-a does not take into consideration any ties, which makes it unpractical. Consider using \code{\link{KendallTauB}} (Tau-b) when ties are present.)
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons,
pp. 57-59.

Hollander, M, Wolfe, D. A., Chicken, E. (2014) \emph{Nonparametric Statistical Methods}, Third edition, Wiley,

Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University Papers Series on Quantitative Applications in the Social Sciences, 07-004. Newbury Park, CA: Sage, pp. 49-56

}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{cor} (method="kendall")} for Tau b, \code{\link{StuartTauC}}, \code{\link{GoodmanKruskalGamma}}\cr
\code{\link{Lambda}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

# Kendall's tau-a C|R
KendallTauA(tab, direction="column", conf.level=0.95)
# Kendall's tau-a R|C
KendallTauA(tab, direction="row", conf.level=0.95)

# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. 1814 (143)
tab <- as.table(cbind(c(11,2),c(4,6)))

KendallTauA(tab, direction="row", conf.level=0.95)
KendallTauA(tab, direction="column", conf.level=0.95)

# Liebetrau, pp. 52
x <- c(1,2,2,3,3,3,4,5)
y <- c(1,3,2,1,5,3,4,5)

ConDisPairs(table(x, y))
KendallTauA(x, y, conf.level=0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
