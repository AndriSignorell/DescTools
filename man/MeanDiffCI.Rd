\name{MeanDiffCI}
\alias{MeanDiffCI}
\title{Confidence Interval For Difference of Means
}
\description{Calculates the confidence interval for the difference of two means either the classical way or with the bootstrap approach.
}
\usage{
MeanDiffCI(x, y, 
           conf.level = 0.95, sides = c("two.sided", "left", "right"), 
           method = c("classic", "boot"), paired = FALSE, var.equal = FALSE, 
           na.rm = FALSE, ...)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a (non-empty) numeric vector of data values.
}
  \item{conf.level}{confidence level of the interval.
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{method}{a vector of character strings representing the type of intervals required. The value should be any subset of the values
  \code{"classic"}, \code{"boot"}. Bootstrap type can be provided by the dots.
  See \code{\link[boot]{boot.ci}}.
}
\item{paired}{a logical indicating whether you want confidence intervals 
for a paired design. Defaults to \code{FALSE}.}

\item{var.equal}{a logical variable indicating whether to treat the 
two variances as being equal. Default is \code{FALSE}. If \code{TRUE} then the pooled variance 
is used to estimate the variance otherwise the Welch 
(or Satterthwaite) approximation to the degrees of freedom is used. Passed on to 
\code{\link{t.test}()}.
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
\item{\dots}{further arguments, can be used to provide further arguments to 
the boot function.}

}
\details{This function collects code from two sources. The classical confidence interval is calculated by means of \code{\link{t.test}}.
The bootstrap intervals are strongly based on the example in \code{\link[boot]{boot}}.
}
\value{a numeric vector with 3 elements:
  \item{meandiff}{the difference: mean(x) - mean(y)}
  \item{lci}{lower bound of the confidence interval}
  \item{uci}{upper bound of the confidence interval}
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{
\code{\link{MeanCI}}, \code{\link{VarCI}}, \code{\link{MedianCI}}, 
\code{\link[boot]{boot.ci}}
}

\examples{
x <- d.pizza$price[d.pizza$driver=="Carter"]
y <- d.pizza$price[d.pizza$driver=="Miller"]

MeanDiffCI(x, y, na.rm=TRUE)
MeanDiffCI(x, y, conf.level=0.99, na.rm=TRUE)

# the different types of bootstrap confints
MeanDiffCI(x, y, method="boot", type="norm", na.rm=TRUE)
MeanDiffCI(x, y, method="boot", type="basic", na.rm=TRUE)
# MeanDiffCI(x, y, method="boot", type="stud", na.rm=TRUE)
MeanDiffCI(x, y, method="boot", type="perc", na.rm=TRUE)
MeanDiffCI(x, y, method="boot", type="bca", na.rm=TRUE)

# for long form variables
with(d.pizza, with(split(price, driver), 
  MeanDiffCI(Carter, Miller) )
)
}
\keyword{univar }

