\name{Gini}
\alias{Gini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gini Coefficient
%%  ~~function to do ... ~~
}
\description{Compute the Gini coefficient, the most commonly used measure of inequality.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gini(x, n = rep(1, length(x)), unbiased = TRUE,
     conf.level = NA, R = 1000, type = "bca", na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector containing at least non-negative elements. The result will be \code{NA}, if x contains negative elements.
%%     ~~Describe \code{x} here~~
}
  \item{n}{a vector of frequencies (weights), must be same length as x.
%%     ~~Describe \code{n} here~~
}
  \item{unbiased}{logical. In order for G to be an unbiased estimate of the true population value,
   calculated gini is multiplied by \eqn{n/(n-1)}{n/(n-1)}. Default is TRUE. (See Dixon, 1987)
%%     ~~Describe \code{unbiased} here~~
}
  \item{conf.level}{confidence level for the confidence interval, restricted to lie between 0 and 1.
  If set to \code{TRUE} the bootstrap confidence intervals are calculated.
  If set to \code{NA} (default) no confidence intervals are returned.

%%     ~~Describe \code{conf.level} here~~
}
  \item{R}{number of bootstrap replicates. Usually this will be a single positive integer.
  For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of
  integers where each component gives the number of resamples from each of the rows of weights.\cr
  This is ignored if no confidence intervals are to be calculated.

%%     ~~Describe \code{R} here~~
}
  \item{type}{character string representing the type of interval required.
  The value should be one out of the c(\code{"norm"},\code{"basic"}, \code{"stud"},
  \code{"perc"} or \code{"bca"}).\cr
  This argument is ignored if no confidence intervals are to be calculated.
%%     ~~Describe \code{type} here~~
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{
The range of the Gini coefficient goes from 0 (no concentration) to \eqn{\sqrt(\frac{n-1}{n})} (maximal concentration). The bias corrected Gini coefficient goes from 0 to 1.\cr
The small sample variance properties of the Gini coefficient are not known, and large sample approximations to the variance of the coefficient are poor (Mills and Zandvakili, 1997; Glasser, 1962; Dixon et al., 1987),
therefore confidence intervals are calculated via bootstrap re-sampling methods (Efron and Tibshirani, 1997). \cr
Two types of bootstrap confidence intervals are commonly used, these are
percentile and bias-corrected (Mills and Zandvakili, 1997; Dixon et al., 1987; Efron and Tibshirani, 1997).
The bias-corrected intervals are most appropriate for most applications. This is set as default for the \code{type} argument (\code{"bca"}).
Dixon (1987) describes a refinement of the bias-corrected method known as 'accelerated' -
this produces values very closed to conventional bias corrected intervals.\cr
(Iain Buchan (2002) \emph{Calculating the Gini coefficient of inequality}, see: \url{http://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm})
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{conf.level} is set to \code{NA} then the result will be
\item{}{a single numeric value} and
if a \code{conf.level} is provided, a named numeric vector with 3 elements:
  \item{gini}{Gini coefficient}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}


\references{
Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.

Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.

Marshall, Olkin (1979) \emph{Inequalities: Theory of Majorization and Its
Applications}. New York: Academic Press.

Glasser C. (1962) Variance formulas for the mean difference and coefficient of concentration.
\emph{Journal of the American Statistical Association} 57:648-654.

Mills JA, Zandvakili A. (1997). Statistical inference via bootstrapping for measures of inequality.
\emph{Journal of Applied Econometrics} 12:133-150.

Dixon, PM, Weiner J., Mitchell-Olds T, Woodley R. (1987) Boot-strapping the Gini coefficient of inequality.
\emph{Ecology} 68:1548-1551.

Efron B, Tibshirani R. (1997) Improvements on cross-validation:
The bootstrap method. \emph{Journal of the American Statistical Association} 92:548-560.

}

\author{Andri Signorell <andri@signorell.net>
}


\seealso{See \code{\link{Herfindahl}}, \code{\link{Rosenbluth}} for concentration measures,
\code{\link{Lc}} for the Lorenz curve\cr
\code{\link[ineq]{ineq}()} in the package \pkg{ineq} contains additional inequality measures}

\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

# compute Gini coefficient
Gini(x)

# working with weights
fl <- c(2.5, 7.5, 15, 35, 75, 150)    # midpoints of classes
n  <- c(25, 13, 10, 5, 5, 2)          # frequencies

# with confidence intervals
Gini(fl, n, conf.level=0.95, unbiased=FALSE)

# some special cases
x <- c(10, 10, 0, 0, 0)
plot(Lc(x))

Gini(x, unbiased=FALSE)

# the same with weights
Gini(x=c(10, 0), n=c(2,3), unbiased=FALSE)

# perfect balance
Gini(c(10, 10, 10))
}
\keyword{univar}

