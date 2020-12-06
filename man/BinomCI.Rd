\name{BinomCI}
\alias{BinomCI}
\title{ Confidence Intervals for Binomial Proportions }
\description{
Compute confidence intervals for binomial proportions following the most popular methods.\cr
(Wald, Wilson, Agresti-Coull, Jeffreys, Clopper-Pearson etc.)
}
\usage{
BinomCI(x, n, conf.level = 0.95, sides = c("two.sided", "left", "right"),
        method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
                   "modified wilson", "wilsoncc","modified jeffreys",
                   "clopper-pearson", "arcsine", "logit", "witting", "pratt", 
                   "midp", "lik"),
        rand = 123)
}
\arguments{
  \item{x}{ number of successes. }
  \item{n}{ number of trials. }
  \item{conf.level}{ confidence level, defaults to 0.95. }
  \item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default),
\code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}.}

  \item{method}{ character string specifing which method to use; this can be one out of:
  \code{"wald"}, \code{"wilson"}, \code{"wilsoncc"}, \code{"agresti-coull"}, \code{"jeffreys"},
  \code{"modified wilson"}, \code{"modified jeffreys"}, \code{"clopper-pearson"},
  \code{"arcsine"}, \code{"logit"}, \code{"witting"}, \code{"pratt"}, \code{"midp"} and \code{"lik"}. Defaults to \code{"wilson"}.
  Abbreviation of method are accepted. See details. }
  \item{rand}{ seed for random number generator; see details. }
}
\details{
  All arguments are being recycled.

  The \bold{Wald } interval is obtained by inverting the acceptance region of the Wald
  large-sample normal test.

  The \bold{Wald with continuity correction } interval is obtained by adding the term 1/(2*n) to the Wald interval.

  The \bold{Wilson} interval, which is the default, was introduced by Wilson (1927) and is
  the inversion of the CLT approximation to the family of equal tail tests of p = p0.
  The Wilson interval is recommended by Agresti and Coull (1998) as well as by
  Brown et al (2001).

  The \bold{Agresti-Coull} interval was proposed by Agresti and Coull (1998) and is a slight
  modification of the Wilson interval. The Agresti-Coull intervals are never shorter
  than the Wilson intervals; cf. Brown et al (2001).

  The \bold{Jeffreys} interval is an implementation of the equal-tailed Jeffreys prior interval
  as given in Brown et al (2001).

  The \bold{modified Wilson} interval is a modification of the Wilson interval for x close to 0
  or n as proposed by Brown et al (2001).

  The \bold{Wilson cc} interval is a modification of the Wilson interval adding a continuity correction term.

  The \bold{modified Jeffreys} interval is a modification of the Jeffreys interval for
  \code{x == 0 | x == 1} and \code{x == n-1 | x == n} as proposed by
  Brown et al (2001).

  The \bold{Clopper-Pearson} interval is based on quantiles of corresponding beta
  distributions. This is sometimes also called exact interval.

  The \bold{arcsine} interval is based on the variance stabilizing distribution for the binomial
  distribution.

  The \bold{logit} interval is obtained by inverting the Wald type interval for the log odds.

  The \bold{Witting} interval (cf. Beispiel 2.106 in Witting (1985)) uses randomization to
  obtain uniformly optimal lower and upper confidence bounds (cf. Satz 2.105 in
  Witting (1985)) for binomial proportions.

  The \bold{Pratt} interval is obtained by extremely accurate normal approximation. (Pratt 1968)
  
  The \bold{Mid-p} approach is used to reduce the conservatism of the Clopper-Pearson, which is known to be very pronounced. The method midp accumulates the tail areas. 
  The lower bound \eqn{p_l} is found as the solution to the equation
  \deqn{\frac{1}{2} f(x;n,p_l) + (1-F(x;m,p_l)) = \frac{\alpha}{2}}
  where \eqn{f(x;n,p)} denotes the probability mass function (pmf) and
  \eqn{F(x;n,p)} the (cumulative) distribution function of the binomial
  distribution with size \eqn{n} and proportion \eqn{p} evaluated at
  \eqn{x}. 
  The upper bound \eqn{p_u} is found as the solution to the equation
  \deqn{\frac{1}{2} f(x;n,p_u) + F(x-1;m,p_u) = \frac{\alpha}{2}}
   In case x=0 then the lower bound is
  zero and in case x=n then the upper bound is 1.

  The \bold{Likelihood-based} approach is said to be theoretically appealing. Confidence intervals are based on profiling the binomial deviance in the neighbourhood of the
MLE.

  For more details we refer to Brown et al (2001) as well as Witting (1985).



Some approaches for the confidence intervals can potentially yield negative results or values beyond 1. These would be reset such as not to exceed the range of [0, 1].

  And now, which interval should we use? The Wald interval often has inadequate coverage, particularly for small n and values of p close to 0 or 1. Conversely, the Clopper-Pearson Exact method is very conservative and tends to produce wider intervals than necessary. Brown et al. recommends the Wilson or Jeffreys methods for small n and Agresti-Coull, Wilson, or Jeffreys, for larger n as providing more reliable coverage than the alternatives. Also note that the point estimate for the Agresti-Coull method is slightly larger than for other methods because of the way this interval is calculated.

}
\value{
  A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.
 }

\note{ The base of this function was \code{binomCI()} in the  \pkg{SLmisc} package. In the meantime the code has been updated on several occasions  and has undergone some additions and bugfixes.
}


\references{
  Agresti A. and Coull B.A. (1998) Approximate is better than "exact" for interval
  estimation of binomial proportions.
  \emph{American Statistician}, \bold{52}, pp. 119-126.

  Brown L.D., Cai T.T. and Dasgupta A. (2001) Interval estimation for a binomial
  proportion \emph{Statistical Science}, \bold{16}(2), pp. 101-133.

  Witting H. (1985) \emph{Mathematische Statistik I}. Stuttgart: Teubner.

  Pratt J. W. (1968) A normal approximation for binomial, F, Beta, and other
common, related tail probabilities \emph{Journal of the American Statistical Association}, 63, 1457-
1483.

Wilcox, R. R. (2005) \emph{Introduction to robust estimation and hypothesis testing}. Elsevier Academic Press

Newcombe, R. G. (1998) Two-sided confidence intervals for the single proportion: comparison of seven methods, \emph{Statistics in Medicine}, 17:857-872
https://pubmed.ncbi.nlm.nih.gov/16206245/

}
\author{ Matthias Kohl <Matthias.Kohl@stamats.de>,
Rand R. Wilcox (Pratt's method), Michael Hoehle <hoehle@math.su.se> (Mid-p), 
Andri Signorell <andri@signorell.net> (interface issues) }

\seealso{\code{\link[stats]{binom.test}}, \code{\link[Hmisc]{binconf}}, \code{\link{MultinomCI}}, \code{\link{BinomDiffCI}}, \code{\link{BinomRatioCI}} }
\examples{
BinomCI(x=37, n=43, method=c("wald", "wilson", "agresti-coull", "jeffreys",
  "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine", "logit",
  "witting", "pratt")
)


# the confidence interval computed by binom.test
#   corresponds to the Clopper-Pearson interval
BinomCI(x=42, n=43, method="clopper-pearson")
binom.test(x=42, n=43)$conf.int


# all arguments are being recycled:
BinomCI(x=c(42, 35, 23, 22), n=43, method="wilson")
BinomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="jeffreys")

# example Table I in Newcombe (1998)
do.call(cbind, lapply(1:4, 
  function(i){
    Format(BinomCI(x      = c(81, 15, 0, 1)[i], 
                   n      = c(263, 148, 20, 29)[i], 
                   method = c("wald", "waldcc", "wilson","wilsoncc", 
                              "clopper-pearson", "midp", "lik"))[, -1], 
           digits=4)
}))

}
\keyword{univar}
