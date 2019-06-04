\name{power.chisq.test}
\alias{power.chisq.test}

\title{Power Calculations for ChiSquared Tests}
\description{
Compute power of test or determine parameters to obtain target
     power (same as \code{\link{power.anova.test}}).}
\usage{
power.chisq.test(n = NULL, w = NULL, df = NULL, sig.level = 0.05, power = NULL)
}

\arguments{
  \item{n}{total number of observations.}
  \item{w}{effect size.}
  \item{df}{degree of freedom (depends on the chosen test.}
  \item{sig.level}{Significance level (Type I error probability).}
  \item{power}{Power of test (1 minus Type II error probability).}
}
\details{
 Exactly one of the parameters \code{w}, \code{n}, \code{power} or
     \code{sig.level} must be passed as NULL, and this parameter is
     determined from the others. Note that the last one has non-NULL
     default, so \code{NULL} must be explicitly passed, if you want to compute
     it.

}
\value{
   Object of class "power.htest", a list of the arguments
     (including the computed one) augmented with 'method' and 'note'
     elements.
}
\references{Cohen, J. (1988) \emph{Statistical power analysis for the
	behavioral sciences (2nd ed.)} Hillsdale, NJ: Lawrence Erlbaum.}
	
\author{Stephane Champely <champely@univ-lyon1.fr> \cr
but this is a mere copy of Peter Dalgaard's work on power.t.test}

\note{  
\code{\link{uniroot}} is used to solve power equation for unknowns, so you may
     see errors from it, notably about inability to bracket the root
     when invalid arguments are given.
}

\seealso{\code{\link{power.t.test}}}

\examples{
## Exercise 7.1 P. 249 from Cohen (1988) 
power.chisq.test(w=0.289, df=(4-1), n=100, sig.level=0.05)

## Exercise 7.3 p. 251
power.chisq.test(w=0.346, df=(2-1)*(3-1), n=140, sig.level=0.01)

## Exercise 7.8 p. 270
power.chisq.test(w=0.1, df=(5-1)*(6-1), power=0.80, sig.level=0.05)
}
\keyword{htest}