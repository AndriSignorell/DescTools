\name{KappaM}
\alias{KappaM}
\title{Kappa for m Raters}
\description{
Computes Fleiss’ kappa, which quantifies agreement among m≥2 raters 
on categorical items by contrasting the mean observed 
agreement with the chance agreement implied by the overall 
category proportions. It equals Scott’s pi when m=2 (but not Cohen’s kappa).
}
\usage{
KappaM(x, method = c("Fleiss", "Conger", "Light"), conf.level = NA)
}
\arguments{
  \item{x}{\eqn{n \times m}{n x m} matrix or dataframe, n subjects m raters.}
  \item{method}{a logical indicating whether the exact Kappa (Conger, 1980), the Kappa described by Fleiss (1971) or Light's Kappa (1971) should be computed.}
\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence intervals will be calculated.
}
}

\details{
Missing data are omitted in a listwise way.\cr
Fleiss’ kappa (Fleiss, 1971) is a multi-rater 
agreement coefficient that generalises 
Scott’s pi (Scott, 1955) to m raters by basing chance agreement on the average 
category proportions across raters. For m = 2 it equals Scott’s pi 
(not Cohen’s kappa). Cohen’s kappa, by contrast, is defined for 
two raters and computes chance agreement from each rater’s own 
marginal distribution; it can also be extended with weights for 
ordinal scales (weighted Cohen’s kappa). Light’s kappa (Light, 1971) 
is simply 
the unweighted mean of all pairwise Cohen kappas among multiple 
raters, whereas Conger’s kappa (Conger, 1980) is a principled multi-rater generalisation
that reduces exactly to Cohen’s kappa when m = 2.\cr 
Standard errors and Wald-type confidence intervals are 
available for all of these coefficients (Cohen, Scott, Fleiss, Conger); 
bootstrap intervals are a practical alternative when 
assumptions are doubtful. }
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}

\references{
Conger, A.J. (1980): Integration and generalisation of Kappas for multiple raters. \emph{Psychological Bulletin}, 88, 322-328

Fleiss, J.L. (1971): Measuring nominal scale agreement among many raters \emph{Psychological Bulletin}, 76, 378-382

Fleiss, J.L., Levin, B., & Paik, M.C. (2003): \emph{Statistical Methods for Rates and Proportions}, 3rd Edition. New York: John Wiley & Sons

Light, R.J. (1971): Measures of response agreement for qualitative 
data: Some generalizations and alternatives. 
\emph{Psychological Bulletin}, 76, 365-377.

Scott, W.A. (1955). Reliability of content analysis: the case of 
nominal scale coding. \emph{Public Opinion Quarterly}, XIX, 321-325. 
}

\note{ This function was previously published as \code{kappam.fleiss()} in the  \pkg{irr} package and has been integrated here with some changes in the interface.
}

\author{Matthias Gamer, with some modifications by Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{CohenKappa}}
}
\examples{
statement <- data.frame(
  A=c(2,3,1,3,1,2,1,2,3,3,3,3,3,2,1,3,3,2,2,1,
      2,1,3,3,2,2,1,2,1,1,2,3,3,3,3,3,1,2,1,1),
  B=c(2,2,2,1,1,2,1,2,3,3,2,3,1,3,1,1,3,2,1,2,
      2,1,3,2,2,2,3,2,1,1,2,2,3,3,3,3,2,2,2,3),
  C=c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,2,2,2,2,3,
      2,2,3,3,2,2,3,2,2,2,2,3,3,3,3,3,3,2,2,2),
  D=c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,3,2,2,2,2,
      3,1,3,2,2,2,1,2,2,1,2,3,3,3,3,3,3,2,2,1),
  E=c(2,2,2,3,3,2,3,1,3,3,2,3,3,3,3,3,2,2,2,3,
      2,3,3,2,2,2,3,2,1,3,2,3,3,1,3,3,3,2,2,1)
)

KappaM(statement)

KappaM(statement, method="Conger")   # Exact Kappa
KappaM(statement, conf.level=0.95)   # Fleiss' Kappa and confidence intervals

KappaM(statement, method="Light")   # Exact Kappa
}
\keyword{multivar}
\concept{ irr }
