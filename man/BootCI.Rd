\name{BootCI}
\alias{BootCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple Bootstrap Confidence Intervals
%%  ~~function to do ... ~~
}
\description{Convenience wrapper for calculating bootstrap confidence intervals for univariate and bivariate statistics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BootCI(x, y = NULL, FUN, ..., bci.method = c("norm", "basic", "stud", "perc", "bca"),
       conf.level = 0.95, sides = c("two.sided", "left", "right"), R = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}

    \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}, when a bivariate statistic is used.
}

  \item{FUN}{the function to be used }


    \item{bci.method}{A vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}, as it is passed on as \code{method} to \code{\link[boot]{boot.ci}}.
}
  \item{conf.level}{confidence level of the interval.
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{...}{further arguments are passed to the function \code{FUN}.
}

\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling,
 some resamples may use one set of weights and others use a different set of weights. In this case \code{R} would be a vector
 of integers where each component gives the number of resamples from each of the rows of weights.
}

%% \item{sd}{the standard deviation of x. If provided it's interpreted as sd of the population and the normal %% quantiles will be used for constructing the confidence intervals. If left to \code{NULL} (default) the
%% sample \code{sd(x)} will be calculated and used in combination with the t-distribution.}

}
\value{
a named numeric vector with 3 elements:
  \item{est}{the specific estimate, as calculated by \code{FUN}}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{MeanCI}}, \code{\link{MedianCI}}
}
\examples{
set.seed(1984)
BootCI(d.pizza$temperature, FUN=mean, na.rm=TRUE, bci.method="basic")
BootCI(d.pizza$temperature, FUN=mean, trim=0.1, na.rm=TRUE, bci.method="basic")

BootCI(d.pizza$temperature, FUN=Skew, na.rm=TRUE, bci.method="basic")

BootCI(d.pizza$operator, d.pizza$area, FUN=CramerV)

spearman <- function(x,y) cor(x, y, method="spearman", use="p")
BootCI(d.pizza$temperature, d.pizza$delivery_min, FUN=spearman)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar}
\keyword{ nonparametric }
