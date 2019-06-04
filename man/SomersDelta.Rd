\name{SomersDelta}
\alias{SomersDelta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Somers' Delta
%%  ~~function to do ... ~~
}
\description{Calculate Somers' Delta statistic, a measure of
association for ordinal factors in a two-way table. The function has interfaces for a table (matrix) and for single vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SomersDelta(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...)
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
  \code{"row"} calculates Somers' D (R | C) ("column dependent").
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
Somers' D(C|R) and Somers' D(R|C) are asymmetric modifications of \eqn{\tau_b} and Goodman-Kruskal's Gamma. C|R indicates that
the row variable x is regarded as the independent variable and the column variable y is regarded
as dependent. Similarly, R|C indicates that the column variable y is regarded as the independent
variable and the row variable x is regarded as dependent. It is logically very similar to Gamma, but differs in that it uses a correction only for pairs that are tied on the dependent variable. As Gamma and the Taus, D is appropriate only when both variables lie on an ordinal scale.\cr
Somers' D is computed as\cr
\deqn{ D(C | R) = \frac{P-Q}{n^2 - \sum(n_i.^2)}}
%% D(C | R) = (P-Q)/(n^2 - sum(rowSums(tab)^2))
where P equals twice the number of concordances and Q twice the number of discordances and \eqn{n_i.} rowSums(tab). Its range lies [-1, 1]. The interpretation of d is analogous to Gamma.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a single numeric value if no confidence intervals are requested\cr
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

Somers, R. H. (1962) A New Asymmetric Measure of Association for Ordinal Variables, \emph{American Sociological Review}, 27, 799--811.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310--364.

}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{There's an implementation of Somers's D in Frank Harrell's \pkg{Hmisc} \code{\link[Hmisc]{somers2}},
which is quite fast for large sample sizes. However it is restricted to computing Somers' Dxy rank correlation between a variable x and a binary (0-1) variable y.\cr
\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{KendallTauA}} (tau-a), \code{\link{KendallTauB}} (tau-b), \code{\link{cor}} (method="kendall") for tau-b, \code{\link{StuartTauC}} (tau-c),
\code{\link{GoodmanKruskalGamma}}\cr
\code{\link{Lambda}}, \code{\link{GoodmanKruskalTau}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

# Somers' D C|R
SomersDelta(tab, direction="column", conf.level=0.95)
# Somers' D R|C
SomersDelta(tab, direction="row", conf.level=0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
