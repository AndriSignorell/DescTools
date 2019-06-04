\name{SignTest}
\alias{SignTest}
\alias{SignTest.default}
\alias{SignTest.formula}

\title{Sign Test}
\description{
Performs one- and two-sample sign tests on vectors of data. 
}
\usage{
SignTest(x, ...)

\method{SignTest}{default}(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
         mu = 0, conf.level = 0.95, \dots )

\method{SignTest}{formula}(formula, data, subset, na.action, \dots)

}


\arguments{
  \item{x}{numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.}
  \item{y}{an optional numeric vector of data values: as with x non-finite values will be omitted.}
  \item{mu}{a number specifying an optional parameter used to form the null hypothesis. See Details.}
  \item{alternative}{is a character string, one of \code{"greater"},
  \code{"less"}, or \code{"two.sided"}, or the initial letter of each,
  indicating the specification of the alternative hypothesis. For
  one-sample tests, \code{alternative} refers to the true
  median of the parent population in relation to the hypothesized
  value of the median.}
  \item{conf.level}{confidence level for the returned confidence
  interval, restricted to lie between zero and one.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}  
  
  \item{\dots}{further arguments to be passed to or from methods.}

}
\details{
The formula interface is only applicable for the 2-sample test. 

\code{SignTest} computes a \dQuote{Dependent-samples Sign-Test} if both
\code{x} and \code{y} are provided.  If only \code{x} is provided,
the \dQuote{One-sample Sign-Test} will be computed.

For the one-sample sign-test, the null hypothesis is
that the median of the population from which \code{x} is drawn is \code{mu}.
For the two-sample dependent case, the null hypothesis is
that the median for the differences of the populations from which \code{x}
and \code{y} are drawn is \code{mu}.
The alternative hypothesis indicates the direction of divergence of the
population median for \code{x} from \code{mu} (i.e., \code{"greater"},
\code{"less"}, \code{"two.sided"}.)

The confidence levels are exact.

}

\value{
A list of class \code{htest}, containing the following components:
  \item{statistic}{ the S-statistic (the number of positive differences
                    between the data and the hypothesized median), with names attribute
                    \dQuote{S}.}
  \item{parameter}{ the total number of valid differences.}                  
  \item{p.value}{   the p-value for the test.}
  \item{null.value}{is the value of the median specified by the null hypothesis. This
                    equals the input argument \code{mu}. }
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{  the type of test applied.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{  a confidence interval for the median.}
  \item{estimate}{  the sample median.}
}

 
\references{
Gibbons, J.D. and Chakraborti, S. (1992):
\emph{Nonparametric Statistical Inference}. Marcel Dekker Inc., New York.

Kitchens, L. J. (2003): \emph{Basic Statistics and Data Analysis}. Duxbury.

Conover, W. J. (1980): \emph{Practical Nonparametric Statistics, 2nd ed}. Wiley, New York.

}

\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{t.test}}, \code{\link{wilcox.test}}, \code{\link{ZTest}}, \code{\link{binom.test}}, 
\code{\link[BSDA]{SIGN.test}} in the package \pkg{BSDA} (reporting approximative confidence intervals).
}

\examples{
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

SignTest(x, y)
wilcox.test(x, y, paired = TRUE)


d.light <- data.frame( 
  black = c(25.85,28.84,32.05,25.74,20.89,41.05,25.01,24.96,27.47),
  white <- c(18.23,20.84,22.96,19.68,19.5,24.98,16.61,16.07,24.59),
  d <- c(7.62,8,9.09,6.06,1.39,16.07,8.4,8.89,2.88)
)

d <- d.light$d

SignTest(x=d, mu = 4)
wilcox.test(x=d, mu = 4, conf.int = TRUE)

SignTest(x=d, mu = 4, alternative="less")
wilcox.test(x=d, mu = 4, conf.int = TRUE, alternative="less")

SignTest(x=d, mu = 4, alternative="greater")
wilcox.test(x=d, mu = 4, conf.int = TRUE, alternative="greater")

# test die interfaces
x <- runif(10)
y <- runif(10)
g <- rep(1:2, each=10) 
xx <- c(x, y)

SignTest(x ~ group, data=data.frame(x=xx, group=g ))
SignTest(xx ~ g)
SignTest(x, y)

SignTest(x - y)
}
\keyword{htest}
