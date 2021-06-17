\name{AndersonDarlingTest}
\alias{AndersonDarlingTest}
\title{
  Anderson-Darling Test of Goodness-of-Fit
}
\description{
  Performs the Anderson-Darling test
  of goodness-of-fit to a specified continuous univariate
  probability distribution.
}
\usage{
AndersonDarlingTest(x, null = "punif", ..., nullname)
}
\arguments{
  \item{x}{
    numeric vector of data values.
  }
  \item{null}{
    a function, or a character string giving the name of a function,
    to compute the cumulative distribution function for the
    null distribution.
  }
  \item{\dots}{
    additional arguments for the cumulative distribution function.
  }
  \item{nullname}{
    optional character string describing the null distribution.\cr
    The default is \code{"uniform distribution"}.
  }
}
\details{
  This command performs the Anderson-Darling test
  of goodness-of-fit to the distribution specified by the argument
  \code{null}. It is assumed that the values in \code{x} are
  independent and identically distributed random values, with some
  cumulative distribution function \eqn{F}.
  The null hypothesis is that \eqn{F} is the function
  specified by the argument \code{null}, while the alternative
  hypothesis is that \eqn{F} is some other function.
  
  The procedures currently implemented are for the case of a SIMPLE null hypothesis, that is, where all the parameters of the distribution are known. Note that other packages such as 'normtest' support the test of a COMPOSITE null hypothesis where some or all of the parameters are unknown leading to different results concerning the test statistic and the p-value. Thus in 'normtest' you can test whether the data come from a normal distribution with some mean and variance (which will be estimated from the same data).

The discrepancies can be large if you don't have a lot of data (say less than 1000 observations).

}
\value{
  An object of class \code{"htest"} representing the result of
  the hypothesis test.
}
\references{
  Anderson, T.W. and Darling, D.A. (1952)
  Asymptotic theory of certain 'goodness-of-fit' criteria based
  on stochastic processes.
  \emph{Annals of Mathematical Statistics} \bold{23}, 193--212.

  Anderson, T.W. and Darling, D.A. (1954)
  A test of goodness of fit.
  \emph{Journal of the American Statistical Association} \bold{49}, 765--769.

  Marsaglia, G. and Marsaglia, J. (2004)
  Evaluating the Anderson-Darling Distribution.
  \emph{Journal of Statistical Software} \bold{9} (2), 1--5.
  February 2004.
  \url{https://www.jstatsoft.org/v09/i02}
}
\author{
  Original C code by George Marsaglia and John Marsaglia.
  \R interface by Adrian Baddeley.
}
\seealso{
  \code{\link{shapiro.test}} and all other tests for normality.
}
\examples{
x <- rnorm(10, mean=2, sd=1)
AndersonDarlingTest(x, "pnorm", mean=2, sd=1)
}
\keyword{htest}
