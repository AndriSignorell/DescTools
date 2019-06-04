\name{HotellingsT2Test}
\alias{HotellingsT2Test}
\alias{HotellingsT2Test.default}
\alias{HotellingsT2Test.formula}

\title{Hotelling's T2 Test}
\description{

Hotelling's T2 test is the multivariate generlisation of the Student's t test. A one-sample Hotelling's T2 test can be used to test if a set of vectors of data (which should be a sample of a single statistical population) has a mean equal to a hypothetical mean. A two-sample Hotelling's T2 test may be used to test for significant differences between the mean vectors (multivariate means) of two multivariate data sets are different.
}

\usage{HotellingsT2Test(x, \dots)

\method{HotellingsT2Test}{default}(x, y = NULL, mu = NULL, test = "f", \dots)

\method{HotellingsT2Test}{formula}(formula, data, subset, na.action, \dots) }

\arguments{
  \item{x}{a numeric data frame or matrix. }
  \item{y}{an optional numeric data frame or matrix for the two sample test. If \code{NULL} a one sample test is performed.}
  \item{mu}{a vector indicating the hypothesized value of the mean (or difference
          in means if a two sample test is performed). \code{NULL} represents origin or no difference between the groups. }
  \item{test}{if \code{"f"}, the decision is based on the F-distribution, if \code{"chi"} a chi-squared approximation is used. }
  \item{formula}{a formula of the form \code{x ~ g} where \code{x} is a numeric matrix giving  the data values and \code{g} a factor
    with two levels giving the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{\dots}{further arguments to be passed to or from methods.}

}

\details{

  The classical test for testing the location of a multivariate population or for testing the mean
  difference for two multivariate populations. When \code{test = "f"} the F-distribution is used for
  the test statistic and it is assumed that the data are normally distributed. If the chisquare
  approximation is used, the normal assumption can be relaxed to existence of second moments.
  In the two sample case both populations are assumed to have the same covariance matrix.

  The formula interface is only applicable for the 2-sample tests.

}

\value{
  A list with class 'htest' containing the following components:
  \item{statistic }{the value of the T2-statistic. (That is the scaled value of the statistic that has an
                    F distribution or a chisquare distribution depending on the value of \code{test}).}
  \item{parameter}{the degrees of freedom for the T2-statistic.}
  \item{p.value}{the p-value for the test.}
  \item{null.value}{the specified hypothesized value of the mean or mean difference
   depending on whether it was a one-sample test or a two-sample test.}
  \item{alternative}{a character string with the value 'two.sided'.}
  \item{method}{a character string indicating what type of test was performed.}
  \item{data.name}{a character string giving the name of the data (and grouping vector).}

}

\references{
Nordhausen K., Sirkia S., Oja H. and Tyler D. E. (2012) \emph{ICSNP: Tools for
  Multivariate Nonparametrics}. R package version 1.0-9.\cr
  \url{https://cran.r-project.org/package=ICSNP}

Anderson, T.W. (2003), \emph{An introduction to
multivariate analysis}, New Jersey: Wiley.  }

\author{Klaus Nordhausen, <klaus.nordhausen@uta.fi>}

\examples{
math.teach <- data.frame(
  teacher = factor(rep(1:2, c(3, 6))),
  satis   = c(1, 3, 2, 4, 6, 6, 5, 5, 4),
  know    = c(3, 7, 2, 6, 8, 8, 10, 10, 6))

with(math.teach,
  HotellingsT2Test(cbind(satis, know) ~ teacher))
}

\keyword{htest }
\keyword{ multivariate }
