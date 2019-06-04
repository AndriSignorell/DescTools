\name{JonckheereTerpstraTest}
\title{Exact Version of Jonckheere-Terpstra Test}
\alias{JonckheereTerpstraTest}
\alias{JonckheereTerpstraTest.default}
\alias{JonckheereTerpstraTest.formula}

\description{
  Jonckheere-Terpstra test to test for ordered differences among classes.
}
\usage{
JonckheereTerpstraTest(x, \dots)

\method{JonckheereTerpstraTest}{default}(x, g, alternative = c("two.sided", "increasing", "decreasing"), 
                       nperm = NULL, \dots)

\method{JonckheereTerpstraTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data vectors.}
  \item{g}{a vector or factor object giving the group for the corresponding elements of x. Ignored if x is a list.}
  
  \item{alternative}{means are monotonic (\code{two.sided}), \code{increasing}, or
    \code{decreasing}}
  \item{nperm}{number of permutations for the reference distribution.
    The default is \code{NULL} in which case the permutation p-value is not
    computed. It's recommended to set \code{nperm} to 1000 or higher if permutation p-value is desired.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}  
  \item{\dots}{further argument to be passed to methods.}
}

\details{
  JonckheereTerpstraTest is the exact (permutation) version of the
  Jonckheere-Terpstra test.  It uses the statistic
  \deqn{\sum_{k<l} \sum_{ij} I(X_{ik} < X_{jl}) + 0.5 I(X_{ik} =
    X_{jl}),} where \eqn{i, j} are observations in groups \eqn{k} and
  \eqn{l} respectively.  The asymptotic version is equivalent to
  \code{cor.test(x, g, method="k")}. The exact calculation requires that there
  be no ties and that the sample size is less than 100. When data are
  tied and sample size is at most 100 permutation p-value is returned.\cr

  If x is a list, its elements are taken as the samples to be compared, and hence have to be numeric data vectors. 
  In this case, g is ignored, and one can simply use JonckheereTerpstraTest(x) to perform the test. 
  If the samples are not yet contained in a list, use JonckheereTerpstraTest(list(x, ...)). \cr

  Otherwise, \code{x} must be a numeric data vector, and \code{g} must be a vector or factor object of the 
  same length as \code{x} giving the group for the corresponding elements of \code{x}. 
  
}

\note{ The function was previously published as \code{jonckheere.test()} in the  \pkg{clinfun} package and has been
integrated here without logical changes. Some argument checks and a formula interface were added.
}

\author{Venkatraman E. Seshan <seshanv@mskcc.org>, minor adaptations Andri Signorell}

\examples{
set.seed(1234)
g <- ordered(rep(1:5, rep(10,5)))
x <- rnorm(50) + 0.3 * as.numeric(g)

JonckheereTerpstraTest(x, g)

x[1:2] <- mean(x[1:2]) # tied data

JonckheereTerpstraTest(x, g)
JonckheereTerpstraTest(x, g, nperm=5000)

# Duller, S. 222
coffee <- data.frame(
  time=c(
  447,396,383,410,
  438,521,468,391,504,472,
  513,543,506,489,407), 
  grp=Untable(c(4,6,5), type="ordered")[,1]
)  

# the formula interface:
JonckheereTerpstraTest(time ~ grp, data=coffee)
}

\references{
  Jonckheere, A. R. (1954). A distribution-free k-sample test again
  ordered alternatives. \emph{Biometrika} 41:133-145.

  Terpstra, T. J. (1952). The asymptotic normality and consistency of
  Kendall's test against trend, when ties are present in one ranking.
  \emph{Indagationes Mathematicae} 14:327-333.
}
\keyword{htest}
