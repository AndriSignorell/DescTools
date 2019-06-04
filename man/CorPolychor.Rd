\name{CorPolychor}
\alias{CorPolychor}
\alias{print.CorPolychor}

\title{Polychoric Correlation}
\description{
  Computes the polychoric correlation (and its standard error)
  between two ordinal variables or from their contingency table, under the 
  assumption that the ordinal variables dissect continuous latent variables that are bivariate normal. Either
  the maximum-likelihood estimator or a (possibly much) quicker ``two-step'' approximation is available. For the ML
  estimator, the estimates of the thresholds and the covariance matrix of the estimates are also available.
}

\usage{
CorPolychor(x, y, ML = FALSE, control = list(), std.err = FALSE, maxcor=.9999)

\method{print}{CorPolychor}(x, digits = max(3, getOption("digits") - 3), ...)

}
\arguments{
  \item{x}{a contingency table of counts or an ordered categorical variable; the latter can be numeric, logical, a factor,
    or an ordered factor, but if a factor, its levels should be in proper order.}
  \item{y}{if \code{x} is a variable, a second ordered categorical variable.}
  \item{ML}{if \code{TRUE}, compute the maximum-likelihood estimate; if \code{FALSE}, the default, compute a quicker
    ``two-step'' approximation.}
  \item{control}{optional arguments to be passed to the \code{optim} function.}
  \item{std.err}{if \code{TRUE}, return the estimated variance of the correlation (for the two-step estimator) 
    or the estimated covariance matrix (for the ML estimator) of the correlation and thresholds; the default is \code{FALSE}.}
  \item{maxcor}{maximum absolute correlation (to insure numerical stability).}
    \item{digits}{integer, determining the number of digits used to format the printed result
}
\item{\dots}{not used}
}

\value{
  If \code{std.err} is \code{TRUE}, 
  returns an object of class \code{"polycor"} with the following components:
  \item{type}{set to \code{"polychoric"}.}
  \item{rho}{the CorPolychoric correlation.}
%  \item{row.cuts}{estimated thresholds for the row variable (\code{x}), for the ML estimate.}
%  \item{col.cuts}{estimated thresholds for the column variable (\code{y}), for the ML estimate.}
  \item{var}{the estimated variance of the correlation, or, for the ML estimate, 
    the estimated covariance matrix of the correlation and thresholds.}
  \item{n}{the number of observations on which the correlation is based.}
  \item{chisq}{chi-square test for bivariate normality.}
  \item{df}{degrees of freedom for the test of bivariate normality.}
  \item{ML}{\code{TRUE} for the ML estimate, \code{FALSE} for the two-step estimate.}
  Othewise, returns the polychoric correlation.
}

\references{
    Drasgow, F. (1986) 
    CorPolychoric and polyserial correlations. 
    Pp. 68--74 in S. Kotz and N. Johnson, eds.,
    \emph{The Encyclopedia of Statistics, Volume 7.} Wiley.
    
    Olsson, U. (1979)
    Maximum likelihood estimation of the CorPolychoric correlation coefficient.
    \emph{Psychometrika} \bold{44}, 443-460.
}
\note{This is a verbatim copy from polchor function in the package polycor.}
\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[polycor]{hetcor}}, \code{\link[polycor]{polyserial}}, \code{\link{print.CorPolychor}}, \code{\link[stats]{optim}}}

\examples{
set.seed(12345)
z <- RndPairs(1000, 0.6)
x <- z[,1]
y <- z[,2]

cor(x, y)                                  # sample correlation
x <- cut(x, c(-Inf, .75, Inf))
y <- cut(y, c(-Inf, -1, .5, 1.5, Inf))

CorPolychor(x, y)                          # 2-step estimate
CorPolychor(x, y, ML=TRUE, std.err=TRUE)   # ML estimate
}

\keyword{models}
