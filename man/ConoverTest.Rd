\name{ConoverTest}
\alias{ConoverTest}
\alias{ConoverTest.default}
\alias{ConoverTest.formula}

\title{Conover's Test of Multiple Comparisons}
\description{
Perform Conover's test of multiple comparisons using rank sums as post hoc test following a significant \code{\link{kruskal.test}}.
}
\usage{
ConoverTest(x, \dots)

\method{ConoverTest}{default}(x, g,
            method = c("holm", "hochberg", "hommel", "bonferroni", "BH",
                       "BY", "fdr", "none"),
            alternative = c("two.sided", "less", "greater"),
            out.list = TRUE, \dots)

\method{ConoverTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{method}{the method for adjusting p-values for multiple comparisons. The function is calling \code{\link{p.adjust}} and this parameter is directly passed through.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.}
  \item{out.list}{logical, indicating if the results should be printed in list mode or as a square matrix. Default is list (TRUE).}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the data values and \code{rhs} the corresponding groups.}
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
  \code{ConoverTest} performs the post hoc pairwise multiple comparisons procedure appropriate to follow the rejection of a Kruskal-Wallis test.
Conover's test is more powerful than Dunn's post hoc multiple comparisons test (\code{\link{DunnTest}}). The interpretation of stochastic dominance requires an assumption that the CDF of one group does not cross the CDF of the other. \cr
ConoverTest makes m = k(k-1)/2 multiple pairwise comparisons based on the Conover-Iman t-test-statistic for the rank-sum differences:
\deqn{\left | \bar{R}_{i}-\bar{R}_{j} \right | > t_{1-\alpha/2, n-k} \cdot \sqrt{ s^2 \cdot \left [
\frac{n-1-\hat{H}^*}{n-k} \right ] \cdot \left [ \frac{1}{n_i} + \frac{1}{n_j} \right ] } }
with the (tie corrected) statistic of the Kruskal Wallis test
\deqn{\hat{H}^* = \frac{\frac{12}{n \cdot (n+1)} \cdot \sum_{i=1}^{k}\frac{R_{i}^2}{n_i} - 3\cdot(n+1) } {1-\frac{\sum_{i=1}^{r} \left ( t_i^3-t_i \right )}{n^3-n}} }
and the \eqn{s^2} being
\deqn{s^2 = \frac{1}{n-1} \cdot \left [ \sum{R_i^2} - n \cdot \frac{(n+1)^2}{4} \right ]}

If \code{x} is a list, its elements are taken as the samples to be
compared, and hence have to be numeric data vectors.  In this case,
\code{g} is ignored, and one can simply use \code{ConoverTest(x)}
to perform the test.  If the samples are not yet contained in a
list, use \code{ConoverTest(list(x, ...))}.

Otherwise, \code{x} must be a numeric data vector, and \code{g} must
be a vector or factor object of the same length as \code{x} giving
the group for the corresponding elements of \code{x}.
}
\value{
  A list with class \code{"DunnTest"} containing the following components:
  \item{res}{an array containing the mean rank differencens and the according p-values}
}
\author{Andri Signorell <andri@signorell.net>, the interface is based on R-Core code}

\references{
Conover W. J., Iman R. L. (1979) On multiple-comparisons procedures, \emph{Tech. Rep.} LA-7677-MS, Los Alamos Scientific Laboratory.

Conover, W. J. (1999) Practical Nonparametric Statistics \emph{Wiley}, Hoboken, NJ. 3rd edition.
}
\seealso{
  \code{\link{DunnTest}}, \code{\link{NemenyiTest}}, \code{\link{kruskal.test}}, \code{\link{wilcox.test}}, \code{\link{p.adjust}}
}
\examples{
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
ConoverTest(list(x, y, z))

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))

# do the kruskal.test first
kruskal.test(x, g)

# ...and the pairwise test afterwards
ConoverTest(x, g)

## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
ConoverTest(Ozone ~ Month, data = airquality)
}
\keyword{htest}
