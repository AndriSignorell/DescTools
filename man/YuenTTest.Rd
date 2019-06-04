\name{YuenTTest}
\alias{YuenTTest}
\alias{YuenTTest.default}
\alias{YuenTTest.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Yuen t-Test For Trimmed Means}
\description{
  Performs one and two sample Yuen t-tests for trimmed means on vectors of data.
}
\usage{

YuenTTest(x, ...)

\method{YuenTTest}{default}(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
          mu = 0, paired = FALSE, conf.level = 0.95, trim = 0.2, \dots )

\method{YuenTTest}{formula}(formula, data, subset, na.action, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.}
  \item{y}{an optional numeric vector of data values: as with x non-finite values will be omitted.}
  \item{alternative}{is a character string, one of \code{"greater"},
  \code{"less"}, or \code{"two.sided"}, or the initial letter of each,
  indicating the specification of the alternative hypothesis. For
  one-sample tests, \code{alternative} refers to the true
  median of the parent population in relation to the hypothesized
  value of the mean.}
  \item{paired}{a logical indicating whether you want a paired z-test.}
  \item{mu}{a number specifying the hypothesized mean of the population. }
  \item{conf.level}{ confidence level for the interval computation. }
  \item{trim}{the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. Values of trim outside that range are taken as the nearest endpoint.}
    \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}  
  \item{\dots}{further arguments to be passed to or from methods.}

}
\value{
An object of class \code{htest} containing the following components: 
\item{statistic}{the value of the t-statistic.} 
\item{parameter}{the degrees of freedom for the t-statistic and the trim percentage used.} 
\item{p.value}{the p-value for the test.} 
\item{conf.int}{a confidence interval for the trimmed mean appropriate to the specified alternative hypothesis.} 
\item{estimate}{the estimated trimmed mean or difference in trimmed means depending on whether it was a one-sample test or a two-sample test.
} 
\item{null.value}{the specified hypothesized value of the trimmed mean or trimmed mean difference depending on whether it was a one-sample test or a two-sample test.} 
\item{alternative}{a character string describing the alternative hypothesis.}
\item{method}{a character string indicating what type of test was performed.} 
\item{data.name}{a character string giving the name(s) of the data.}
  
}

\references{
Wilcox, R. R. (2005) Introduction to robust estimation and hypothesis testing. \emph{Academic Press}.\cr
Yuen, K. K. (1974) The two-sample trimmed t for unequal population variances. \emph{Biometrika}, 61, 165-170.
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Andri Signorell <andri@signorell.net>, based on R-Core code of \code{\link{t.test}}
}


\seealso{ \code{\link{t.test}}, \code{\link{print.htest}} }
\examples{
x <- rnorm(25, 100, 5)
YuenTTest(x, mu=99)

# the classic interface
with(sleep, YuenTTest(extra[group == 1], extra[group == 2]))

# the formula interface
YuenTTest(extra ~ group, data = sleep)


# Stahel (2002), pp. 186, 196  
d.tyres <- data.frame(A=c(44.5,55,52.5,50.2,45.3,46.1,52.1,50.5,50.6,49.2),
                      B=c(44.9,54.8,55.6,55.2,55.6,47.7,53,49.1,52.3,50.7))
with(d.tyres, YuenTTest(A, B, paired=TRUE))


d.oxen <- data.frame(ext=c(2.7,2.7,1.1,3.0,1.9,3.0,3.8,3.8,0.3,1.9,1.9),
                     int=c(6.5,5.4,8.1,3.5,0.5,3.8,6.8,4.9,9.5,6.2,4.1))
with(d.oxen, YuenTTest(int, ext, paired=FALSE))
}

\keyword{ htest }

