\name{CoefVar}
\alias{CoefVar}
\alias{CoefVar.lm}
\alias{CoefVar.aov}
\alias{CoefVar.default}
\alias{CoefVarCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coefficient of Variation
%%  ~~function to do ... ~~
}
\description{Calculates the coefficient of variation and its confidence limits using various methods.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CoefVar(x, ...)

\method{CoefVar}{lm}(x, unbiased = FALSE, na.rm = FALSE, ...)

\method{CoefVar}{aov}(x, unbiased = FALSE, na.rm = FALSE, ...)

\method{CoefVar}{default}(x, weights = NULL, unbiased = FALSE,
       na.rm = FALSE, ...)

CoefVarCI(K, n, conf.level = 0.95, 
          sides = c("two.sided", "left", "right"),
          method = c("nct","vangel","mckay","verrill","naive"))       

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{unbiased}{logical value determining, if a bias correction should be used (see. details). Default is FALSE. %% Reference???
%%     ~~Describe \code{unbiased} here~~
}
  \item{K}{the coefficient of variation as calculated by \code{CoefVar()}.
%%     ~~Describe \code{x} here~~
}
  \item{n}{the number of observations used for calculating the coefficient of variation.}

  \item{conf.level}{confidence level of the interval. Defaults to 0.95.
%%     ~~Describe \code{conf.level} here~~
}
 \item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default),
\code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}.}
\item{method}{character string specifing the method to use for calculating the confidence intervals, can be one out of:
  \code{"nct"} (default), \code{"vangel"}, \code{"mckay"}, \code{"verrill"} (currently not yet implemented) and \code{"naive"}. Abbreviation of method is accepted. See details.}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
\item{\dots}{further arguments (not used here).}
}
\details{
In order for the coefficient of variation to be an unbiased estimate of the true population value,
the coefficient of variation  is corrected as:   \deqn{ CV_{korr} = CV \cdot \left( 1 - \frac{1}{4\cdot(n-1)} + \frac{1}{n} \cdot CV^2 + \frac{1}{2 \cdot (n-1)^2} \right) }

For determining\verb{ }\bold{the confidence intervals}\verb{ } for the coefficient of variation a number of methods have been proposed. \code{CoefVarCI()} currently supports five different methods.
The details for the methods are given in the specific references.

The \bold{"naive" method} \verb{     } 
is based on dividing the standard confidence limit for the standard deviation by the sample mean.

\bold{McKay's} \verb{     } 
approximation is asymptotically exact as n goes to infinity. McKay recommends this approximation only if the coefficient of variation is less than 0.33. Note that if the coefficient of variation is greater than 0.33, either the normality of the data is suspect or the probability of negative values in the data is non-neglible. In this case, McKay's approximation may not be valid. Also, it is generally recommended that the sample size should be at least 10 before using McKay's approximation.

\bold{Vangel's modified McKay method} \verb{     } 
is more accurate than the McKay in most cases, particilarly for small samples.. According to Vangel, the unmodified McKay is only more accurate when both the coefficient of variation and alpha are large. However, if the coefficient of variation is large, then this implies either that the data contains negative values or the data does not follow a normal distribution. In this case, neither the McKay or the modified McKay should be used.
In general, the Vangel's modified McKay method is recommended over the McKay method. It generally provides good approximations as long as the data is approximately normal and the coefficient of variation is less than 0.33. This is the default method.

See also: https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm

\bold{nct}   \verb{     }uses the noncentral t-distribution to calculate the confidence intervals. See Smithson (2003).

\bold{Note:}\verb{    }
  Analytic (precision) weights are not supported. For likelihood-based
weighted variance estimation, see \code{\link[stats]{cov.wt}}.


}
\value{if no confidence intervals are requested:
the estimate as numeric value (without any name)\cr\cr
else a named numeric vector with 3 elements
\item{est}{estimate}
\item{lwr.ci}{lower confidence interval}
\item{upr.ci}{upper confidence interval}
}

\references{
McKay, A. T. (1932). Distribution of the coefficient of variation and the extended
\emph{t} distribution, \emph{Journal of the Royal Statistical Society}, \emph{95}, 695--698.

Johnson, B. L., Welch, B. L. (1940). Applications of the non-central t-distribution. \emph{Biometrika}, 31, 362--389.

Mark Vangel (1996) Confidence Intervals for a Normal Coefficient of Variation, \emph{American Statistician}, Vol. 15, No. 1, pp. 21-26.

Kelley, K. (2007). Sample size planning for the coefcient of variation from the accuracy in parameter estimation approach. \emph{Behavior Research Methods, 39} (4), 755-766

Kelley, K. (2007). Constructing confidence intervals for standardized effect sizes: Theory, application, and implementation. \emph{Journal of Statistical Software, 20} (8), 1-24

Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41

Steve Verrill (2003) Confidence Bounds for Normal and Lognormal Distribution Coefficients of Variation, \emph{Research Paper 609}, USDA Forest Products Laboratory, Madison, Wisconsin.

Verrill, S. and Johnson, R.A. (2007) Confidence Bounds and Hypothesis Tests for Normal Distribution Coefficients of Variation, \emph{Communications in Statistics Theory and Methods}, Volume 36, No. 12, pp 2187-2206.
}
\author{Andri Signorell <andri@signorell.net>, \cr
Michael Smithson <michael.smithson@anu.edu.au> (noncentral-t)
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Mean}}, \code{\link{SD}},   (both supporting weights)
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(15)
x <- runif(100)
CoefVar(x, conf.level=0.95)

#       est    low.ci    upr.ci
# 0.5092566 0.4351644 0.6151409

# Coefficient of variation for a linear model
r.lm <- lm(Fertility ~ ., swiss)
CoefVar(r.lm)

# the function is vectorized, so arguments are recyled...
# https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm
CoefVarCI(K = 0.00246, n = 195, method="vangel", 
          sides="two.sided", conf.level = c(.5,.8,.9,.95,.99,.999))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
