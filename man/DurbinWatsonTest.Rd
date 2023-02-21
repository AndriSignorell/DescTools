\name{DurbinWatsonTest}
\alias{DurbinWatsonTest}
\encoding{latin1}
\title{Durbin-Watson Test}
\description{
 Performs the Durbin-Watson test for autocorrelation of disturbances.
}
\usage{
DurbinWatsonTest(formula, order.by = NULL,
                 alternative = c("greater", "two.sided", "less"),
                 iterations = 15, exact = NULL, tol = 1e-10, data = list())
}
\arguments{
  \item{formula}{a symbolic description for the model to be tested
    (or a fitted \code{"lm"} object).}
 \item{order.by}{Either a vector \code{z} or a formula with a single explanatory
    variable like \code{~ z}. The observations in the model
    are ordered by the size of \code{z}. If set to \code{NULL} (the
    default) the observations are assumed to be ordered (e.g., a
    time series).}
 \item{alternative}{a character string specifying the alternative hypothesis.}
 \item{iterations}{an integer specifying the number of iterations
    when calculating the p-value with the "pan" algorithm.}
 \item{exact}{logical. If set to \code{FALSE} a normal approximation
   will be used to compute the p value, if \code{TRUE} the "pan"
   algorithm is used. The default is to use "pan" if the sample size
   is < 100.}
 \item{tol}{tolerance. Eigenvalues computed have to be greater than
            \code{tol} to be treated as non-zero.}
 \item{data}{an optional data frame containing the variables in the model.
    By default the variables are taken from the environment which \code{DurbinWatsonTest}
    is called from.}
}
\details{The Durbin-Watson test has the null hypothesis that the autocorrelation
of the disturbances is 0. It is possible to test against the alternative that it is
greater than, not equal to, or less than 0, respectively. This can be specified
by the \code{alternative} argument.

Under the assumption of normally distributed disturbances, the null distribution
of the Durbin-Watson statistic is the distribution of a linear
combination of chi-squared variables. The p-value is computed using the
Fortran version of Applied Statistics Algorithm AS 153 by Farebrother
(1980, 1984). This algorithm is called "pan" or "gradsol". For large sample
sizes the algorithm might fail to compute the p value; in that case a warning
is printed and an approximate p value will be given; this p value is computed
using a normal approximation with mean and variance of the Durbin-Watson test
statistic.

Examples can not only be found on this page, but also on the help pages of the
data sets \code{\link[lmtest]{bondyield}}, \code{\link[lmtest]{currencysubstitution}},
\code{\link[lmtest]{growthofmoney}}, \code{\link[lmtest]{moneydemand}},
\code{\link[lmtest]{unemployment}}, \code{\link[lmtest]{wages}}.

For an overview on R and econometrics see Racine & Hyndman (2002).
}
\value{An object of class \code{"htest"} containing:
  \item{statistic}{the test statistic.}
  \item{p.value}{the corresponding p-value.}
  \item{method}{a character string with the method used.}
  \item{data.name}{a character string with the data name.}
}
\note{ This function was previously published as \code{dwtest} in the  \pkg{lmtest} package and has been integrated here without logical changes.
}
\author{Torsten Hothorn, Achim Zeileis, Richard W. Farebrother (pan.f), Clint Cummins (pan.f), Giovanni Millo, David Mitchell}

\references{

J. Durbin & G.S. Watson (1950),
Testing for Serial Correlation in Least Squares Regression I.
\emph{Biometrika} \bold{37}, 409--428.

J. Durbin & G.S. Watson (1951),
Testing for Serial Correlation in Least Squares Regression II.
\emph{Biometrika} \bold{38}, 159--178.

J. Durbin & G.S. Watson (1971),
Testing for Serial Correlation in Least Squares Regression III.
\emph{Biometrika} \bold{58}, 1--19.

R.W. Farebrother (1980),
Pan's Procedure for the Tail Probabilities of the
Durbin-Watson Statistic (Corr: 81V30 p189; AS R52: 84V33 p363- 366; AS
R53: 84V33 p366- 369).
\emph{Applied Statistics} \bold{29},  224--227.

R. W. Farebrother (1984),
[AS R53] A Remark on Algorithms AS 106 (77V26 p92-98), AS 153 (80V29 p224-227)
  and AS 155: The Distribution of a Linear Combination of \eqn{\chi^2} Random
  Variables (80V29 p323-333)
\emph{Applied Statistics} \bold{33}, 366--369.

W. Krämer & H. Sonnberger (1986),
\emph{The Linear Regression Model under Test}. Heidelberg: Physica.

J. Racine & R. Hyndman (2002),
Using R To Teach Econometrics.
\emph{Journal of Applied Econometrics} \bold{17}, 175--189.

}

\seealso{\code{\link{lm}}}

\examples{

## generate two AR(1) error terms with parameter
## rho = 0 (white noise) and rho = 0.9 respectively
err1 <- rnorm(100)

## generate regressor and dependent variable
x <- rep(c(-1,1), 50)
y1 <- 1 + x + err1

## perform Durbin-Watson test
DurbinWatsonTest(y1 ~ x)

err2 <- stats::filter(err1, 0.9, method="recursive")
y2 <- 1 + x + err2
DurbinWatsonTest(y2 ~ x)

## for a simple vector use:
e_t <- c(-32.33, -26.603, 2.215, -16.967, -1.148, -2.512, -1.967, 11.669,
         -0.513, 27.032, -4.422, 40.032, 23.577, 33.94, -2.787, -8.606,
          0.575, 6.848, -18.971, -29.063)
DurbinWatsonTest(e_t ~ 1)
}
\keyword{htest}
