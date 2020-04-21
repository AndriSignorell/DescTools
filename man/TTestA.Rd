\name{TTestA}
\alias{TTestA}
\title{Student's t-Test Based on Sample Statistics}
\description{
  Performs one and two sample t-tests based on user supplied summary information instead of data as in \code{t.test()}.
}

\usage{
TTestA(mx, sx, nx, my = NULL, sy = NULL, ny = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)
}

  \arguments{
  \item{mx}{a single number representing the sample mean of x.}
  \item{my}{an optional single number representing the sample mean of y.}
  \item{sx}{a single number representing the sample standard deviation of x.}
  \item{sy}{an optional single number representing the sample standard deviation of y.}
  \item{nx}{a single number representing the sample size of x.}
  \item{ny}{an optional single number representing the sample size of y.}
  \item{alternative}{a character string specifying the alternative
  hypothesis, must be one of \code{"two.sided"} (default),
  \code{"greater"} or \code{"less"}.  You can specify just the initial
  letter.}
  \item{mu}{a number indicating the true value of the mean (or
  difference in means if you are performing a two sample test).}
  \item{paired}{paired = \code{TRUE} is not supported here and only present for consistency of arguments. Use one-sample-test for the differences instead.}
  \item{var.equal}{a logical variable indicating whether to treat the
  two variances as being equal. If \code{TRUE} then the pooled
  variance is used to estimate the variance otherwise the Welch
  (or Satterthwaite) approximation to the degrees of freedom is used.}
  \item{conf.level}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to or from methods.}
  }
  \details{
  \code{alternative = "greater"} is the alternative that \code{x} has a
  larger mean than \code{y}.

  If \code{paired} is \code{TRUE} then both \code{mx, sx} and \code{my, sy} must
  be specified and \code{nx} must be equal to \code{ny}.  If
  \code{var.equal} is \code{TRUE} then the pooled estimate of the
  variance is used.  By default, if \code{var.equal} is \code{FALSE}
  then the variance is estimated separately for both groups and the
  Welch modification to the degrees of freedom is used.

  If the input data are effectively constant (compared to the larger of the
  two means) an error is generated.
  }
  \value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the t-statistic.}
  \item{parameter}{the degrees of freedom for the t-statistic.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval for the mean appropriate to the
  specified alternative hypothesis.}
  \item{estimate}{the estimated mean or difference in means depending on
  whether it was a one-sample test or a two-sample test.}
  \item{null.value}{the specified hypothesized value of the mean or mean
  difference depending on whether it was a one-sample test or a
  two-sample test.}
  \item{alternative}{a character string describing the alternative
  hypothesis.}
  \item{method}{a character string indicating what type of t-test was
  performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  }
  \seealso{
  \code{\link{t.test}}
  }

\examples{
## Classical example: Student's sleep data
mx <- 0.75
my <- 2.33
sx <- 1.789010
sy <- 2.002249
nx <- ny <- 10
TTestA(mx=mx, my=my, sx=sx, sy=sy, nx=nx, ny=ny)

# compare to
with(sleep, t.test(extra[group == 1], extra[group == 2]))

# use the one sample test for the differences instead of paired=TRUE option
x <- with(sleep, extra[group == 1])
y <- with(sleep, extra[group == 2])

TTestA(mx=mean(x-y), sx=sd(x-y), nx=length(x-y))

# compared to 
t.test(x, y, paired = TRUE)
}
\keyword{htest}

