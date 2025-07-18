% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LeveneTest.R
\name{LeveneTest}
\alias{LeveneTest}
\alias{LeveneTest.formula}
\alias{LeveneTest.default}
\title{Levene's Test for Homogeneity of Variance}
\usage{
LeveneTest(x, ...)

\method{LeveneTest}{formula}(formula, data, subset, na.action, ...)

\method{LeveneTest}{default}(x, g, center = median, ...)
}
\arguments{
\item{x}{response variable for the default method, or a \code{lm} or
\code{formula} object. If \code{y} is a linear-model object or a formula,
the variables on the right-hand-side of the model must all be factors and
must be completely crossed.}

\item{...}{arguments to be passed down, e.g., \code{data} for the
\code{formula}; can also be used to pass arguments to
the function given by \code{center} (e.g., \code{center=mean},
\code{trim=0.1} specify the 10\% trimmed mean).}

\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives
the data values and \code{rhs} the corresponding groups.}

\item{data}{an optional matrix or data frame (or similar: see
\code{\link{model.frame}}) containing the variables in the formula
\code{formula}.  By default the variables are taken from
\code{environment(formula)}.}

\item{subset}{an optional vector specifying a subset of observations to be used.}

\item{na.action}{a function which indicates what should happen
when the data contain NAs. Defaults to \code{getOption("na.action")}.}

\item{g}{factor defining groups.}

\item{center}{The name of a function to compute the center of each group;
\code{mean} gives the original Levene's test; the default, \code{median},
provides a more robust test (Brown-Forsythe-Test).}
}
\value{
An object of class "htest" representing the result of the
hypothesis test.
}
\description{
Computes Levene's test for homogeneity of variance across groups.
}
\details{
Let \eqn{X_{ij}} be the jth observation of X for the ith group.
Let \eqn{Z_{ij} = |X_{ij} - X_i|}, where \eqn{X_i} is the mean of X in the ith group.
Levene’s test statistic is
\deqn{ W_0 = \frac{ \sum_i n_i (\bar{Z}_i - \bar{Z})^2 / (g - 1) }{ \sum_i 
\sum_j (Z_{ij} - \bar{Z}_i)^2 / \sum_i (n_i - 1) } }
where \eqn{n_i} is the number of observations in group i and g is the number of
groups.
}
\note{
This function is rewritten using common R standards based on
car::leveneTest() using the same calculation logic.
}
\examples{

## example from ansari.test:
## Hollander & Wolfe (1973, p. 86f):
## Serum iron determination using Hyland control sera
serum <- ToLong(data.frame(
          ramsay=c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
                   101, 96, 97, 102, 107, 113, 116, 113, 110, 98),
          jung.parekh=c(107, 108, 106, 98, 105, 103, 110, 105, 104,
                        100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
          ))

LeveneTest(x ~ grp, data=serum)
LeveneTest(x ~ grp, data=serum, center=mean)
LeveneTest(x ~ grp, data=serum, center=mean, trim=0.1)

LeveneTest( c(rnorm(10), rnorm(10, 0, 2)), 
            factor(rep(c("A","B"), each=10)) )

LeveneTest(Ozone ~ Month, data = airquality)

LeveneTest(count ~ spray, data = InsectSprays)
# Compare this to fligner.test() and bartlett.test()

}
\references{
Fox, J. (2008) \emph{Applied Regression Analysis and Generalized
Linear Models}, Second Edition. Sage.

Fox, J. and Weisberg, S. (2011) \emph{An R Companion to Applied Regression},
Second Edition, Sage.

Levene, H. (1960) Robust tests for equality of variances.
in Ingram, O., Hotelling, H. et al. (Hrsg.) (1960) Contributions
to Probability and Statistics, \emph{Essays in Honor of Harold Hotelling}.
Stanford University Press, 1960, ISBN 0-8047-0596-8, S. 278–292.
}
\seealso{
\code{\link{fligner.test}} for a rank-based (nonparametric)
\eqn{k}-sample test for homogeneity of variances; \code{\link{mood.test}}
for another rank-based two-sample test for a difference in scale parameters;
\code{\link{var.test}} and \code{\link{bartlett.test}} for parametric tests
for the homogeneity in variance.

\code{\link[coin:ScaleTests]{ansari_test}} in package \pkg{coin} for exact
and approximate \emph{conditional} p-values for the Ansari-Bradley test, as
well as different methods for handling ties.
}
\author{
andri.signorell \email{andri@signorell.net}; original version
written by John Fox \email{jfox@mcmaster.ca} based on a generic version
contributed by Derek Ogle\cr adapted from a response posted by Brian Ripley
to the r-help email list.
}
\keyword{htest}
