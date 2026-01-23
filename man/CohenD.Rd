\name{CohenD}
\alias{CohenD}
\title{Cohen's and Hedges' Effect Size
}
\description{Computes the Cohen's d and Hedges' g effect size statistics. 
}
\usage{
CohenD(x, y = NULL, 
       conf.level = NA, sides = c("two.sided", "left", "right"),
       correct = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a (non-empty) numeric vector of data values.
}

  \item{conf.level}{confidence level of the interval. 
  Set this to \code{NA}, if no 
  confidence intervals should be calculated. (This is the default) 
}
  \item{sides}{a character string specifying the side of the confidence interval, 
    must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. 
    You can specify just the initial letter. \code{"left"} would be analogue to a 
    hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{correct}{logical, indicating whether to apply the 
  Hedges correction. (Default: \code{FALSE})
}

 \item{na.rm}{logical. Should missing values be removed? 
 Defaults to \code{FALSE}.
}

}
\value{a numeric vector with 3 elements:
  \item{est}{the effect size d}
  \item{lci}{lower bound of the confidence interval}
  \item{uci}{upper bound of the confidence interval}
}
\references{
Cohen, J. (1988) \emph{Statistical power analysis for the behavioral sciences (2nd ed.)} Academic Press, New York.

Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis} Academic Press, Orlando, FL 

Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41
}
\author{Andri Signorell <andri@signorell.net>, William Revelle  <revelle@northwestern.edu> (CI) 
}

\seealso{\code{\link{GlassDelta}}, \code{\link{mean}}, \code{\link{var}}
}
\examples{
x <- d.pizza$price[d.pizza$driver=="Carter"]
y <- d.pizza$price[d.pizza$driver=="Miller"]

CohenD(x, y, conf.level=0.95, na.rm=TRUE)

}

