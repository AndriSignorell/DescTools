\name{KendallTauB}
\alias{KendallTauB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kendall \eqn{\tau_{b}}{Tau-b}}
\description{Calculate Kendall's tau-b. The estimator could also be calculated with \code{cor(..., method="kendall")}.
The calculation of confidence intervals however would not be found there.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
KendallTauB(x, y = NULL, conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, matrix or data.frame.
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

Kendall, M. (1955) \emph{Rank Correlation Methods}, Second Edition, London: Charles Griffin and Co.

Brown, M.B.andBenedetti, J.K.(1977) Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables, \emph{Journal of the American Statistical Association}, 72, 309-315.
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{\code{\link{ConDisPairs}} yields concordant and discordant pairs \cr\cr
Other association measures: \cr
\code{\link{GoodmanKruskalGamma}}, \code{\link{KendallTauA}} (tau-a), \code{\link{cor}} (method="kendall") for tau-b, \code{\link{StuartTauC}} (tau-c), \code{\link{SomersDelta}}\cr
\code{\link{Lambda}}, \code{\link{GoodmanKruskalTau}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}


%% ~~objects to See Also as \code{\link{help}}, ~~~
}\examples{
# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

KendallTauB(tab, conf.level=0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar}
\keyword{nonparametric}
