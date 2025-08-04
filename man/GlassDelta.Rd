\name{GlassDelta}
\alias{GlassDelta}
\title{  Glass' Delta  }
\description{
Glass' Delta is a measure of effect size, used to express the 
magnitude of difference between two groups, particularly when you're 
comparing a treatment group to a control group.
Glass’s delta is appropriate when the standard deviations are significantly 
different between the populations, as it uses only the reference group's 
standard deviation.
}
\usage{
GlassDelta(x, y, conf.level = NULL, use_control_sd = TRUE, na.rm = FALSE)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a (non-empty) numeric vector of data values.
}
  \item{conf.level}{confidence level of the interval. Set this to \code{NA}, 
  if no confidence intervals should be calculated. (This is the default)
}
  \item{use_control_sd}{logical, used as to set the reference. 
  By default (\code{TRUE}), the second group is the reference group.
}
 \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}

}
\value{a numeric vector with 3 elements:
  \item{d}{the effect size d}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}
\references{
Glass, G. V. (1976). Primary, secondary, and meta-analysis of research. 
\emph{Educational Researcher, 5}, 3–8.
}
\author{Ken Kelley <KKelley@ND.Edu>, Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{CohenD}}
}
\examples{
x <- d.pizza$price[d.pizza$driver=="Carter"]
y <- d.pizza$price[d.pizza$driver=="Miller"]

GlassDelta(x, y, conf.level=0.95, na.rm=TRUE)

}
