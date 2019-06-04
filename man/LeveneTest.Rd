\name{LeveneTest}
\alias{LeveneTest}
\alias{LeveneTest.formula}
\alias{LeveneTest.lm}
\alias{LeveneTest.default}

\title{Levene's Test for Homogeneity of Variance}
\description{
 Computes Levene's test for homogeneity of variance across groups.
}
\usage{
LeveneTest(y, ...)

\method{LeveneTest}{formula}(formula, data, ...)
\method{LeveneTest}{lm}(y, ...)
\method{LeveneTest}{default}(y, group, center=median, ...)
}

\arguments{
  \item{y}{response variable for the default method, or a \code{lm} or
  \code{formula} object. If \code{y} is a linear-model object or a formula,
  the variables on the right-hand-side of the model must all be factors and
  must be completely crossed.}
  \item{group}{factor defining groups.}
  \item{center}{The name of a function to compute the center of each group;
  	\code{mean} gives the original Levene's test; the default, \code{median},
  	provides a more robust test (Brown-Forsythe-Test). }

  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the data values and \code{rhs} the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}


  \item{...}{arguments to be passed down, e.g., \code{data} for the
  	\code{formula} and \code{lm} methods; can also be used to pass arguments to
  	the function given by \code{center} (e.g., \code{center=mean} and
  	\code{trim=0.1} specify the 10\% trimmed mean).}
}
%%  \item{subset}{an optional vector specifying a subset of observations
%%    to be used.}
%%  \item{na.action}{a function which indicates what should happen when
%%    the data contain \code{NA}s.  Defaults to
%%    \code{getOption("na.action")}.}

\value{
returns an object meant to be printed showing the results of the test.
}
\note{ This function was previously published as leveneTest() in the library(car)
  and has been integrated here without logical changes.
}
\references{
  Fox, J. (2008)
  \emph{Applied Regression Analysis and Generalized Linear Models},
  Second Edition. Sage.

  Fox, J. and Weisberg, S. (2011)
  \emph{An R Companion to Applied Regression}, Second Edition, Sage.
}
\seealso{
  \code{\link{fligner.test}} for a rank-based (nonparametric)
  \eqn{k}-sample test for homogeneity of variances;
  \code{\link{mood.test}} for another rank-based two-sample test for a
  difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity in variance.

  \code{\link[coin:ScaleTests]{ansari_test}} in package {coin}
  for exact and approximate \emph{conditional} p-values for the
  Ansari-Bradley test, as well as different methods for handling ties.
}

\author{John Fox \email{jfox@mcmaster.ca}; original generic version
    contributed by Derek Ogle\cr
    adapted from a response posted by Brian Ripley to the r-help email list.}

\examples{
## example from ansari.test:
## Hollander & Wolfe (1973, p. 86f):
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)

LeveneTest( c(ramsay, jung.parekh),
  factor(c(rep("ramsay",length(ramsay)), rep("jung.parekh",length(jung.parekh)))))

LeveneTest( c(rnorm(10), rnorm(10, 0, 2)), factor(rep(c("A","B"),each=10)) )

\dontrun{
# original example from package car

with(Moore, LeveneTest(conformity, fcategory))
with(Moore, LeveneTest(conformity, interaction(fcategory, partner.status)))

LeveneTest(conformity ~ fcategory * partner.status, data = Moore)
LeveneTest(conformity ~ fcategory * partner.status, data = Moore, center = mean)
LeveneTest(conformity ~ fcategory * partner.status, data = Moore, center = mean, trim = 0.1)

LeveneTest(lm(conformity ~ fcategory*partner.status, data = Moore))
}
}

\keyword{htest}
