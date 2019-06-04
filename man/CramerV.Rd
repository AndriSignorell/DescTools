\name{Association measures}
\alias{Phi}
\alias{ContCoef}
\alias{CramerV}
\alias{YuleQ}
\alias{YuleY}
\alias{TschuprowT}

\title{
Cramer's V, Pearson's Contingency Coefficient and Phi Coefficient\cr
Yule's Q and Y, Tschuprow's T
}
\description{
Calculate Cramer's V, Pearson's contingency coefficient and phi,
Yule's Q and Y and Tschuprow's T of \code{x}, if \code{x} is a table. If both, \code{x} and \code{y} are given, then the according table will be built first.
}
\usage{
Phi(x, y = NULL, \dots)
ContCoef(x, y = NULL, correct = FALSE, \dots)
CramerV(x, y = NULL, conf.level = NA,
        method = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"), \dots)

YuleQ(x, y = NULL, \dots)
YuleY(x, y = NULL, \dots)
TschuprowT(x, y = NULL, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{can be a numeric vector, a matrix or a table.
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, ...)} is calculated.
}
  \item{conf.level}{confidence level of the interval. This is only implemented for Cramer's V. If set to \code{NA} (which is the       default) no confidence interval will be calculated. \cr
  See examples for calculating bootstrap intervals.
}
\item{method}{string defining the method to calculate confidence intervals for Cramer's V. One out of \code{"ncchisq"} (using noncentral chisquare), \code{"ncchisqadj"}, \code{"fisher"} (using fisher z transformation), \code{"fisheradj"} (using fisher z transformation and bias correction). Default is \code{"ncchisq"}.}
  \item{correct}{logical. This argument only applies to \code{ContCoef} and indicates, whether the Sakoda's adjusted Pearson's C should be returned. Default is \code{FALSE}.
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \code{useNA}.
  }
}
\details{
For x either a matrix or two vectors \code{x} and \code{y} are expected. In latter case \code{table(x, y, ...)} is calculated.
The function handles \code{NAs} the same way the \code{table} function does, so tables are by default calculated with \code{NAs} omitted. \cr\cr
A provided matrix is interpreted as a contingency table, which seems in the case of frequency data the natural interpretation
(this is e.g. also what \code{\link{chisq.test}} expects). \cr\cr
Use the function \code{\link{PairApply}} (pairwise apply) if the measure should be calculated pairwise for all columns.
This allows matrices of association measures to be calculated the same way \code{cor} does. \code{NAs} are by default omitted pairwise,
which corresponds to the \code{pairwise.complete} option of \code{\link{cor}}.
Use \code{\link{complete.cases}}, if only the complete cases of a \code{data.frame} are to be used. (see examples)


The maximum value for Phi is \eqn{\sqrt(min(r, c) - 1)}. The contingency coefficient goes from 0 to \eqn{\sqrt(\frac{min(r, c) - 1}{min(r, c)})}. For the corrected contingency coefficient and for Cramer's V the range is 0 to 1. \cr A Cramer's V in the range of [0, 0.3] is considered as weak, [0.3,0.7] as medium and > 0.7 as strong.
The minimum value for all is 0 under statistical independence.
}
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}
\references{
  Yule, G. Uday (1912) On the methods of measuring association between two attributes. \emph{Journal of the Royal Statistical Society, LXXV}, 579-652

Tschuprow, A. A. (1939) \emph{Principles of the Mathematical Theory of Correlation}, translated by M. Kantorowitsch. W. Hodge & Co.

Cramer, H. (1946) \emph{Mathematical Methods of Statistics}. Princeton University Press

Agresti, Alan (1996) \emph{Introduction to categorical data analysis}. NY: John Wiley and Sons

Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency Tables,
  \emph{Proceedings of the Social Statistics Section of the American Statistical Association} (Part III), 777-780.

Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41

}
\author{
Andri Signorell <andri@signorell.net>, \cr
Michael Smithson <michael.smithson@anu.edu.au>  (confidence intervals for Cramer V)
}

\seealso{
\code{\link{table}}, \code{\link{PlotCorr}}, \code{\link{PairApply}}, \code{\link{Assocs}}
%\url{http://faculty.chass.ncsu.edu/garson/PA765/assocnominal.htm} this is outdated...
}
\examples{
tab <- table(d.pizza$driver, d.pizza$wine_delivered)
Phi(tab)
ContCoef(tab)
CramerV(tab)
TschuprowT(tab)

# just x and y
CramerV(d.pizza$driver, d.pizza$wine_delivered)

# data.frame
PairApply(d.pizza[,c("driver","operator","area")], CramerV, symmetric = TRUE)


# useNA is passed to table
PairApply(d.pizza[,c("driver","operator","area")], CramerV,
          useNA="ifany", symmetric = TRUE)

d.frm <- d.pizza[,c("driver","operator","area")]
PairApply(d.frm[complete.cases(d.frm),], CramerV, symmetric = TRUE)


m <- as.table(matrix(c(2,4,1,7), nrow=2))
YuleQ(m)
YuleY(m)


# Bootstrap confidence intervals for Cramer's V
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, p. 1821

tab <- as.table(rbind(
  c(26,26,23,18, 9),
  c( 6, 7, 9,14,23)))
d.frm <- Untable(tab)

n <- 1000
idx <- matrix(sample(nrow(d.frm), size=nrow(d.frm) * n, replace=TRUE), ncol=n, byrow=FALSE)
v <- apply(idx, 2, function(x) CramerV(d.frm[x,1], d.frm[x,2]))
quantile(v, probs=c(0.025,0.975))

# compare this to the analytical ones
CramerV(tab, conf.level=0.95)
}

\keyword{ multivariate }
