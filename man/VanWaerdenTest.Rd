
\name{VanWaerdenTest}
\alias{VanWaerdenTest}
\alias{VanWaerdenTest.default}
\alias{VanWaerdenTest.formula}
\title{van der Waerden Test}
\description{
  Performs a van der Waerden normal scores test.
}
\usage{
VanWaerdenTest(x, \dots)
  
\method{VanWaerdenTest}{default}(x, g, \dots)

\method{VanWaerdenTest}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.  Non-numeric elements of a list will be coerced, with a
    warning.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored with a warning if
    \code{x} is a list.}
  \item{formula}{a formula of the form \code{response ~ group} where
    \code{response} gives the data values and \code{group} a vector or
    factor of the corresponding groups.} 
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
  \code{VanWaerdenTest} performs a van der Waerden test of the
  null that the location parameters of the distribution of \code{x}
  are the same in each group (sample). The alternative is that they
  differ in at least one.
  
  The van der Waerden rank scores are defined  as the ranks of data, i.e., \eqn{R[i], i = 1, 2, ..., n}, divided by \eqn{1 + n} transformed to a normal score by applying the inverse of the normal distribution function, i.e., \eqn{\Phi^(-1)(R[i]/(1 + n))}. The ranks of data are obtained by ordering the observations from all groups (the same way as \code{\link{kruskal.test}} does it).
  
  If \code{x} is a list, its elements are taken as the samples to be
  compared, and hence have to be numeric data vectors.  In this case,
  \code{g} is ignored, and one can simply use \code{VanWaerdenTest(x)}
  to perform the test.  If the samples are not yet contained in a
  list, use \code{VanWaerdenTest(list(x, ...))}.
  
  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving
  the group for the corresponding elements of \code{x}.
}
\value{
  A list with class \code{"htest"} containing the following components:
    \item{statistic}{the van der Waerden statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"van-der-Waerden normal scores test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
Conover, W. J., Iman, R. L. (1979). On multiple-comparisons procedures, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.

Conover, W. J. (1999). \emph{Practical Nonparameteric Statistics} (Third Edition ed.). Wiley. pp. 396406. 
}

\seealso{
  \code{\link[coin:LocationTests]{normal_test}} in package
  \CRANpkg{coin} where the test is implemented in a more general context (but has a quite unpractical interface).
}
\examples{
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis

VanWaerdenTest(list(x, y, z))

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))
VanWaerdenTest(x, g)

## Formula interface.
require(graphics)
boxplot(Ozone ~ Month, data = airquality)
VanWaerdenTest(Ozone ~ Month, data = airquality)
}
\keyword{htest}
