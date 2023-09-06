\name{GoodmanKruskalTau}
\alias{GoodmanKruskalTau}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Goodman Kruskal's Tau
}
\description{Calculate Goodman Kruskal's tau statistic, a measure of
association for ordinal factors in a two-way table.\cr
The function has interfaces for a table (matrix) and for single vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GoodmanKruskalTau(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...)
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
  \code{"row"} calculates Goodman Kruskal's tau-a (R|C) ("column dependent").
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
Goodman-Kruskal tau measures association for cross tabulations of nominal level variables.
Goodman-Kruskal tau is based on random category assignment. It measures the percentage improvement in predictability of the dependent variable (column or row variable) given the value of other variables (row or column variables). Goodman-Kruskal tau is the same as Goodman-Kruskal lambda except the calculations of the tau statistic are based on assignment probabilities specified by marginal or conditional proportions.
Misclassification probabilities are based on random category assignment with probabilities specified by marginal or conditional proportion.

Goodman Kruskal tau reduces to \eqn{\phi^2} (see: \code{\link{Phi}}) in the 2x2-table case.\cr
}
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons,
pp. 57-59.

Goodman, L. A., & Kruskal, W. H. (1954) Measures of
association for cross classifications. \emph{Journal of the
American Statistical Association}, 49, 732-764.

Somers, R. H. (1962) A New Asymmetric Measure of Association for Ordinal Variables, \emph{American Sociological Review}, 27, 799-811.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310-364.

Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University Papers Series on Quantitative Applications in the Social Sciences, 07-004. Newbury Park, CA: Sage, pp. 24--30

}

\author{Andri Signorell <andri@signorell.net>, based on code from Antti Arppe <antti.arppe@helsinki.fi>
%%  ~~who you are~~
}

\seealso{\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{KendallTauA}} (Tau a), \code{\link{cor}} (method="kendall") for Tau b, \code{\link{StuartTauC}}, \code{\link{GoodmanKruskalGamma}}\cr
\code{\link{Lambda}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

# Goodman Kruskal's tau C|R
GoodmanKruskalTau(tab, direction="column", conf.level=0.95)
# Goodman Kruskal's tau R|C
GoodmanKruskalTau(tab, direction="row", conf.level=0.95)

# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. 1814 (143)
tab <- as.table(cbind(c(11,2),c(4,6)))

GoodmanKruskalTau(tab, direction="row", conf.level=0.95)
GoodmanKruskalTau(tab, direction="column", conf.level=0.95)
# reduce both to:
Phi(tab)^2


# example 1 in Liebetrau (1983)

tt <- matrix(c(549,93,233,119,225,455,402,  
               212,124,78,42,41,12,132,
               54,54,33,13,46,7,153), ncol=3,
             dimnames=list(rownames=c("Gov", "Mil", "Edu", "Eco", "Intel", "Rel", "For"), 
                           colnames=c("One", "Two", "Multi")))

GoodmanKruskalTau(tt, direction = "row", conf.level = 0.95)
GoodmanKruskalTau(tt, direction = "column", conf.level = 0.95)


# SPSS
ttt <- matrix(c(225,53,206,3,1,12), nrow=3,
              dimnames=list(rownames=c("right","center", "left"), 
                            colnames=c("us","ussr")))

round(GoodmanKruskalTau(ttt, direction = "r", con=0.95), d=3)
round(GoodmanKruskalTau(ttt, direction = "c"), d=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
