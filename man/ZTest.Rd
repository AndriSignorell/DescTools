\name{ZTest}
\alias{ZTest}
\alias{ZTest.default}
\alias{ZTest.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Z Test for Known Population Standard Deviation }
\description{
  Compute the test of hypothesis and compute confidence interval on the
  mean of a population when the standard deviation of the population is known.
}
\usage{

ZTest(x, ...)

\method{ZTest}{default}(x, y = NULL, alternative = c("two.sided", "less", "greater"),
      paired = FALSE, mu = 0, sd_pop, conf.level = 0.95, \dots )

\method{ZTest}{formula}(formula, data, subset, na.action, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.}
  \item{y}{an optional numeric vector of data values: as with x non-finite values will be omitted.}
  \item{mu}{a number specifying the hypothesized mean of the population. }
  \item{sd_pop}{a number specifying the known standard deviation of the population. }

  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter. \cr For
  one-sample tests, \code{alternative} refers to the true
  mean of the parent population in relation to the hypothesized
  value of the mean.}

  \item{paired}{a logical indicating whether you want a paired z-test.}
  \item{conf.level}{ confidence level for the interval computation. }
    \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and \code{rhs}
    a factor with two levels giving the corresponding groups.}

  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s. Defaults to \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}

}
\details{
  Most introductory statistical texts introduce inference by using the z-test
  and z-based confidence intervals based on knowing the population
  standard deviation. However statistical packages often do not include
  functions to do z-tests since the t-test is usually more appropriate
  for real world situations. This function is meant to be used during
  that short period of learning when the student is learning about
  inference using z-procedures, but has not learned the t-based
  procedures yet.  Once the student has learned about the
  t-distribution the \code{t.test()} function should be used instead of this
  one (but the syntax is very similar, so this function should be an
  appropriate introductory step to learning \code{t.test()}).

  The formula interface is only applicable for the 2-sample tests.

}
\value{
  A list with class "\code{htest}" containing the following components:
    \item{statistic}{ the value of the z-statistic.}
  \item{p.value}{   the p-value for the test}
  \item{conf.int}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
  \item{estimate}{the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test.}
  \item{null.value}{the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{  a character string indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}

}

\references{
Stahel, W. (2002) \emph{Statistische Datenanalyse, 4th ed}, vieweg
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Andri Signorell <andri@signorell.net>, based on R-Core code of \code{\link{t.test}},\cr
documentation partly from Greg Snow <greg.snow@imail.org>}



\seealso{ \code{\link{t.test}}, \code{\link{print.htest}} }
\examples{
x <- rnorm(25, 100, 5)
ZTest(x, mu=99, sd_pop=5)

# the classic interface
with(sleep, ZTest(extra[group == 1], extra[group == 2], sd_pop=2))

# the formula interface
ZTest(extra ~ group, data = sleep, sd_pop=2)


# Stahel (2002), pp. 186, 196

d.tyres <- data.frame(A=c(44.5,55,52.5,50.2,45.3,46.1,52.1,50.5,50.6,49.2),
                      B=c(44.9,54.8,55.6,55.2,55.6,47.7,53,49.1,52.3,50.7))
with(d.tyres, ZTest(A, B, sd_pop=3, paired=TRUE))


d.oxen <- data.frame(ext=c(2.7,2.7,1.1,3.0,1.9,3.0,3.8,3.8,0.3,1.9,1.9),
                     int=c(6.5,5.4,8.1,3.5,0.5,3.8,6.8,4.9,9.5,6.2,4.1))
with(d.oxen, ZTest(int, ext, sd_pop=1.8, paired=FALSE))
}
\keyword{ htest }

