\name{MeanDiffCI}
\alias{MeanDiffCI}
\alias{MeanDiffCI.formula}
\alias{MeanDiffCI.default}
\title{Confidence Interval For Difference of Means
}
\description{Calculates the confidence interval for the difference of two means either the classical way or with the bootstrap approach.
}
\usage{
MeanDiffCI(x, ...)

\method{MeanDiffCI}{default}(x, y, method = c("classic", "norm", "basic", "stud", "perc", "bca"),
           conf.level = 0.95, sides = c("two.sided", "left", "right"), 
           paired = FALSE, var.equal = FALSE, na.rm = FALSE, R = 999, ...)

\method{MeanDiffCI}{formula}(formula, data, subset, na.action, ...)

}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a (non-empty) numeric vector of data values.
}
  \item{method}{a vector of character strings representing the type of intervals required. The value should be any subset of the values
  \code{"classic"}, \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}.
  See \code{\link[boot]{boot.ci}}.
}
  \item{conf.level}{confidence level of the interval.
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

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
  \item{R}{the number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of integers where each component gives the number of resamples from each of the rows of weights.
  See \code{\link[boot]{boot}}.
}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} is a numeric variable giving the data values and \code{rhs} a factor with two levels giving the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NAs}. Defaults to \code{getOption("na.action")}.}
  \item{\dots}{further argument to be passed to or from methods.}
}
\details{This function collects code from two sources. The classical confidence interval is calculated by means of \code{\link{t.test}}.
The bootstrap intervals are strongly based on the example in \code{\link[boot]{boot}}.
}
\value{a numeric vector with 3 elements:
  \item{meandiff}{the difference: mean(x) - mean(y)}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
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
MeanDiffCI(x, y, method="norm", na.rm=TRUE)
MeanDiffCI(x, y, method="basic", na.rm=TRUE)
# MeanDiffCI(x, y, method="stud", na.rm=TRUE)
MeanDiffCI(x, y, method="perc", na.rm=TRUE)
MeanDiffCI(x, y, method="bca", na.rm=TRUE)

# the formula interface
MeanDiffCI(price ~ driver, data=d.pizza, subset=driver \%in\% c("Carter","Miller"))
}
\keyword{univar }

