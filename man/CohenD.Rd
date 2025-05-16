\name{CohenD}
\alias{CohenD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cohen's Effect Size
%%  ~~function to do ... ~~
}
\description{Computes the Cohen's d and Hedges' g effect size statistics. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CohenD(x, y = NULL, pooled = TRUE, correct = FALSE, conf.level = NA, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{y} here~~
}
  \item{pooled}{logical, indicating whether compute pooled standard deviation or the whole sample standard deviation. Default is TRUE.
%%     ~~Describe \code{pooled} here~~
}
  \item{correct}{logical, indicating whether to apply the Hedges correction. (Default: FALSE)
%%     ~~Describe \code{correct} here~~
}
  \item{conf.level}{confidence level of the interval. Set this to NA, if no confidence intervals should be calculated. (This is the default) 
%%     ~~Describe \code{conf.level} here~~
}
 \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}

}
\value{a numeric vector with 3 elements:
  \item{d}{the effect size d}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}
\references{
Cohen, J. (1988) \emph{Statistical power analysis for the behavioral sciences (2nd ed.)} Academic Press, New York.

Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis} Academic Press, Orlando, FL 

Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>, William Revelle  <revelle@northwestern.edu> (CI) 
%%  ~~who you are~~
}

\seealso{\code{\link{mean}}, \code{\link{var}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- d.pizza$price[d.pizza$driver=="Carter"]
y <- d.pizza$price[d.pizza$driver=="Miller"]

CohenD(x, y, conf.level=0.95, na.rm=TRUE)

}

