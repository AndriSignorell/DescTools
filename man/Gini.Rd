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
Gini(x, conf.level = NA, sides = c("two.sided", "left", "right"),
     method = c("boot"), unbiased = TRUE, weights = NULL, 
     na.rm = FALSE, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector containing at least non-negative elements. The result will be \code{NA}, if x contains negative elements.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level for the confidence interval, restricted to lie between 0 and 1.
  If set to \code{TRUE} the bootstrap confidence intervals are calculated.
  If set to \code{NA} (default) no confidence intervals are returned.
}
  \item{sides}{a character string specifying the side of the confidence interval, 
    must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. 
    You can specify just the initial letter. \code{"left"} would be analogue to a 
    hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{method}{only bootstrap method is implemented. }

  \item{unbiased}{logical. In order for G to be an unbiased estimate of the true population value,
   calculated gini is multiplied by \eqn{n/(n-1)}{n/(n-1)}. Default is TRUE. (See Dixon, 1987)
}
  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
}
  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped 
  before the computation proceeds. Defaults to \code{FALSE}.}

  \item{\dots}{the dots are passed to the function \code{\link[boot]{boot}}, 
   when confidence intervals are calculated.
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
(Iain Buchan (2002) \emph{Calculating the Gini coefficient of inequality}, see: \url{https://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm})
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{conf.level} is set to \code{NA} then the result will be
\item{a}{ single numeric value} and
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
Gini(x=fl, weights=n, conf.level=0.95, unbiased=FALSE)

# some special cases
x <- c(10, 10, 0, 0, 0)
plot(Lc(x))

Gini(x, unbiased=FALSE)

# the same with weights
Gini(x=c(10, 0), weights=c(2,3), unbiased=FALSE)

# perfect balance
Gini(c(10, 10, 10))
}
\keyword{univar}

