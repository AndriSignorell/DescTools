% File src/library/stats/man/VarTest.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{VarTest}
\alias{VarTest}
\alias{VarTest.default}
\alias{VarTest.formula}
\title{ChiSquare Test for One Variance and F Test to Compare Two Variances}
\description{
  Performs either a one sample ChiSquare test to compare the variance of a vector with a given value or an F test to compare the variances of two samples from normal populations.
}
\usage{
VarTest(x, \dots)

\method{VarTest}{default}(x, y,
        alternative = c("two.sided", "less", "greater"),
        ratio = 1, sigma.squared = 1,
        conf.level = 0.95, \dots)

\method{VarTest}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x, y}{numeric vectors of data values.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{ratio}{the hypothesized ratio of the population variances of
    \code{x} and \code{y}.}
  \item{sigma.squared}{a number indicating the true value of the variance, if one sample test is requested.}
  \item{conf.level}{confidence level for the returned confidence
    interval.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} a factor
    with two levels giving the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
The formula interface is only applicable for the 2-sample tests.

  The null hypothesis is that the ratio of the variances of the
  populations from which \code{x} and \code{y} were drawn, or in the
  data to which the linear models \code{x} and \code{y} were fitted, is
  equal to \code{ratio}.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the F test statistic.}
  \item{parameter}{the degrees of the freedom of the F distribution of
    the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the ratio of the population
    variances.}
  \item{estimate}{the ratio of the sample variances of \code{x} and
    \code{y}.}
  \item{null.value}{the ratio of population variances under the null.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the character string
    \code{"F test to compare two variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\author{Andri Signorell <andri@signorell.net> (One sample test)\cr
Two Sample test and help text from R-Core.}
\seealso{
\code{\link{var.test}}, \code{\link{bartlett.test}} for testing homogeneity of variances in
  more than two samples from normal distributions;
  \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank
  based (nonparametric) two-sample tests for difference in scale.
}
\examples{
x <- rnorm(50, mean = 0, sd = 2)

# One sample test
VarTest(x, sigma.squared = 2.5)

# two samples
y <- rnorm(30, mean = 1, sd = 1)
VarTest(x, y)                  # Do x and y have the same variance?
VarTest(lm(x ~ 1), lm(y ~ 1))  # The same.
}
\keyword{htest}
