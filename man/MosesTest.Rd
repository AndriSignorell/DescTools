\name{MosesTest}
\alias{MosesTest}
\alias{MosesTest.default}
\alias{MosesTest.formula}
\title{Moses Test of Extreme Reactions
}
\description{Perform Moses test of extreme reactions, which is a distribution-free non-parametric test for the difference between two independent groups in the extremity of scores (in both directions) that the groups contain. 
Scores from both groups are pooled and converted to ranks, and the test statistic is the span of scores (the range plus 1) in one of the groups chosen arbitrarily. An exact probability is computed for the span and then recomputed after dropping a specified number of extreme scores from each end of its range. The exact one-tailed probability is calculated. }
\usage{
MosesTest(x, ...)

\method{MosesTest}{default}(x, y, extreme = NULL, \dots)

\method{MosesTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{numeric vector of data values. \code{x} will be treated as control group. Non-finite (e.g. infinite or missing) values will be omitted.
}
  \item{y}{numeric vector of data values. \code{y} will be treated as experiment group. Non-finite (e.g. infinite or missing) values will be omitted.
}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s. Defaults to \code{getOption("na.action")}.}
\item{extreme}{integer, defines the number of extreme values to be dropped from the control group before calculating the
span. Default (\code{NULL}) is the integer part of \code{0.05 * length(x)} or \code{1}, whichever is greater. If extreme is too large, it will be cut down to \code{floor(length(x)-2)/2}. }
  \item{\dots}{further arguments to be passed to or from methods.}

}
\details{
For two independent samples from a continuous field, this tests whether extreme values
are equally likely in both populations or if they are more likely to occur in the population
from which the sample with the larger range was drawn.

Note that the ranks are calculated in decreasing mode.

}
\value{
  A list with class \dQuote{htest} containing the following components:
  \item{statistic}{the value of the Moses Test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string \dQuote{Moses Test of Extreme Reactions}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}


\references{
Moses, L.E. (1952) A Two-Sample Test, \emph{Psychometrika}, 17, 239-247.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{ \code{\link{wilcox.test}}, \code{\link{ks.test}} }

\examples{
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

MosesTest(x, y)


set.seed(1479)
x <- sample(1:20, 10, replace=TRUE)
y <- sample(5:25, 6, replace=TRUE)

MosesTest(x, y)
}
\keyword{ htest }
