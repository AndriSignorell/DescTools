% File src/library/stats/man/GTest.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{GTest}
\alias{GTest}
\concept{goodness-of-fit}
\title{G-Test for Count Data}
  \description{
  \code{GTest} performs chi-squared contingency table tests
  and goodness-of-fit tests.
  }
  \usage{
GTest(x, y = NULL, correct = c("none", "williams", "yates"),
      p = rep(1/length(x), length(x)), rescale.p = FALSE) % , simulate.p.value = FALSE, B = 2000
  }
  \arguments{
  \item{x}{a numeric vector or matrix. \code{x} and \code{y} can also
  both be factors.}
  \item{y}{a numeric vector; ignored if \code{x} is a matrix.  If
  \code{x} is a factor, \code{y} should be a factor of the same length.}
  \item{correct}{one out of \code{"none"} (default), \code{"williams"}, \code{"yates"} . See Details. }
  \item{p}{a vector of probabilities of the same length of \code{x}.
  An error is given if any entry of \code{p} is negative.}
  \item{rescale.p}{	
a logical scalar; if \code{TRUE} then p is rescaled (if necessary) to sum to 1. If rescale.p is \code{FALSE}, and p does not sum to 1, an error is given.}

%  \item{rescale.p}{a logical scalar; if \code{TRUE} then \code{p} is rescaled
%  (if necessary) to sum to 1.  If \code{rescale.p} is FALSE, and
%  \code{p} does not sum to 1, an error is given.}

%  \item{simulate.p.value}{a logical indicating whether to compute
%  p-values by Monte Carlo simulation.}

%  \item{B}{an integer specifying the number of replicates used in the
%  Monte Carlo test.}
  }
  \details{
The G-test is also called "Likelihood Ratio Test" and is asymptotically equivalent to the Pearson ChiSquare-test but not usually used when analyzing 2x2 tables. It is used in logistic regression and loglinear modeling which involves contingency tables. The G-test is also reported in the standard summary of \code{Desc} for tables.

  If \code{x} is a matrix with one row or column, or if \code{x} is a
  vector and \code{y} is not given, then a \emph{goodness-of-fit test}
  is performed (\code{x} is treated as a one-dimensional
  contingency table).  The entries of \code{x} must be non-negative
  integers.  In this case, the hypothesis tested is whether the
  population probabilities equal those in \code{p}, or are all equal if
  \code{p} is not given.

  If \code{x} is a matrix with at least two rows and columns, it is
  taken as a two-dimensional contingency table: the entries of \code{x}
  must be non-negative integers.  Otherwise, \code{x} and \code{y} must
  be vectors or factors of the same length; cases with missing values
  are removed, the objects are coerced to factors, and the contingency
  table is computed from these.  Then G-test is
  performed on the null hypothesis that the joint distribution of the
  cell counts in a 2-dimensional contingency table is the product of the
  row and column marginals.



%  If \code{simulate.p.value} is \code{FALSE}, the p-value is computed
%  from the asymptotic chi-squared distribution of the test statistic;
%  continuity correction is only used in the 2-by-2 case (if \code{correct}
%                                                         is \code{TRUE}, the default).  Otherwise the p-value is computed for a
%  Monte Carlo test (Hope, 1968) with \code{B} replicates.

%  In the contingency table case simulation is done by random sampling
%  from the set of all contingency tables with given marginals, and works
%  only if the marginals are strictly positive.  (A C translation of the
%                                                 algorithm of Patefield (1981) is used.)  Continuity correction is
%  never used, and the statistic is quoted without it.  Note that this is
%  not the usual sampling situation assumed for the chi-squared test but
%  rather that for Fisher's exact test.

%  In the goodness-of-fit case simulation is done by random sampling from
%  the discrete distribution specified by \code{p}, each sample being
%  of size \code{n = sum(x)}.  This simulation is done in \R and may be
%  slow.
  Test of independence Yates' correction taken from Mike Camann's 2x2 G-test function.
  Goodness of Fit Yates' correction as described in Zar (2000).
  }
  \value{
  A list with class \code{"htest"} containing the following
  components:
  \item{statistic}{the value the chi-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate
  chi-squared distribution of the test statistic, \code{NA} if the
  p-value is computed by Monte Carlo simulation.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
  performed, and whether Monte Carlo simulation or continuity
  correction was used.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
  }
  \author{Pete Hurd <phurd@ualberta.ca>, Andri Signorell <andri@signorell.net> (tiny tweaks)}
  \seealso{
  \code{\link{chisq.test}}.
  }
  \references{
  Hope, A. C. A. (1968)
  A simplified Monte Carlo significance test procedure.
  \emph{J. Roy, Statist. Soc. B} \bold{30}, 582--598.

  Patefield, W. M. (1981)
  Algorithm AS159.  An efficient method of generating r x c tables
  with given row and column totals.
  \emph{Applied Statistics} \bold{30}, 91--97.

  Agresti, A. (2007)
  \emph{An Introduction to Categorical Data Analysis, 2nd ed.},
  New York: John Wiley & Sons.
  Page 38.

Sokal, R. R., F. J. Rohlf (2012) \emph{Biometry: the principles and practice of statistics in biological research}. 4th edition. W. H. Freeman and Co.: New York. 937 pp.

  }
\examples{

## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
dimnames(M) <- list(gender=c("M","F"),
                    party=c("Democrat","Independent", "Republican"))

(Xsq <- GTest(M))   # Prints test summary

Xsq$observed        # observed counts (same as M)
Xsq$expected        # expected counts under the null


## Testing for population probabilities
## Case A. Tabulated data
x <- c(A = 20, B = 15, C = 25)
GTest(x)
GTest(as.table(x))             # the same
x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
try(
GTest(x, p = p)                # gives an error
)
# works
p <- c(0.40,0.20,0.20,0.19,0.01)
# Expected count in category 5
# is 1.86 < 5 ==> chi square approx.
GTest(x, p = p)                # maybe doubtful, but is ok!

## Case B. Raw data
x <- trunc(5 * runif(100))
GTest(table(x))                # NOT 'GTest(x)'!
}
\keyword{htest}
\keyword{distribution}


