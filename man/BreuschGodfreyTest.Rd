\name{BreuschGodfreyTest}
\alias{BreuschGodfreyTest}
\title{Breusch-Godfrey Test}
\description{
 \code{BreuschGodfreyTest} performs the Breusch-Godfrey test for higher-order
 serial correlation.
}
\usage{
BreuschGodfreyTest(formula, order = 1, order.by = NULL, type = c("Chisq", "F"),
                   data = list(), fill = 0)
}
\arguments{
  \item{formula}{a symbolic description for the model to be tested
    (or a fitted \code{"lm"} object).}
  \item{order}{integer. maximal order of serial correlation to be tested.}
 \item{order.by}{Either a vector \code{z} or a formula with a single explanatory
    variable like \code{~ z}. The observations in the model
    are ordered by the size of \code{z}. If set to \code{NULL} (the
    default) the observations are assumed to be ordered (e.g., a
    time series).}
  \item{type}{the type of test statistic to be returned. Either
    \code{"Chisq"} for the Chi-squared test statistic or
    \code{"F"} for the F test statistic.}
  \item{data}{an optional data frame containing the variables in the
    model. By default the variables are taken from the environment
    which \code{BreuschGodfreyTest} is called from.}
  \item{fill}{starting values for the lagged residuals in the auxiliary
    regression. By default \code{0} but can also be set to \code{NA}.}
  }
\details{
  Under \eqn{H_0} the test statistic is asymptotically Chi-squared with
  degrees of freedom as given in \code{parameter}.
  If \code{type} is set to \code{"F"} the function returns
  a finite sample version of the test statistic, employing an \eqn{F}
  distribution with degrees of freedom as given in \code{parameter}.

  By default, the starting values for the lagged residuals in the auxiliary
  regression are chosen to be 0 (as in Godfrey 1978) but could also be
  set to \code{NA} to omit them.

  \code{BreuschGodfreyTest} also returns the coefficients and estimated covariance
  matrix from the auxiliary regression that includes the lagged residuals.
  Hence, \code{CoefTest} (package: RegClassTools) can be used to inspect the results. (Note,
  however, that standard theory does not always apply to the standard errors
  and t-statistics in this regression.)
}
\value{
 A list with class \code{"BreuschGodfreyTest"} inheriting from \code{"htest"} containing the
 following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{parameter}{degrees of freedom.}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{coefficients}{coefficient estimates from the auxiliary regression.}
  \item{vcov}{corresponding covariance matrix estimate.}
}
\note{ This function was previously published as \code{bgtest} in the  \pkg{lmtest} package and has been integrated here without logical changes.
}

\references{
  Johnston, J. (1984): \emph{Econometric Methods}, Third Edition, McGraw Hill
  Inc.

  Godfrey, L.G. (1978): `Testing Against General Autoregressive and
  Moving Average Error Models when the Regressors Include Lagged
  Dependent Variables', \emph{Econometrica}, 46, 1293-1302.

  Breusch, T.S. (1979): `Testing for Autocorrelation in Dynamic Linear
  Models', \emph{Australian Economic Papers}, 17, 334-355.
}
\author{David Mitchell <david.mitchell@dotars.gov.au>, Achim Zeileis}

\seealso{\code{\link[DescTools]{DurbinWatsonTest}}}
\examples{
## Generate a stationary and an AR(1) series
x <- rep(c(1, -1), 50)

y1 <- 1 + x + rnorm(100)

## Perform Breusch-Godfrey test for first-order serial correlation:
BreuschGodfreyTest(y1 ~ x)

## or for fourth-order serial correlation
BreuschGodfreyTest(y1 ~ x, order = 4)

## Compare with Durbin-Watson test results:
DurbinWatsonTest(y1 ~ x)

y2 <- stats::filter(y1, 0.5, method = "recursive")
BreuschGodfreyTest(y2 ~ x)
}
\keyword{htest}
