\name{KappaM}
\alias{KappaM}
\title{Kappa for m Raters}
\description{
Computes kappa as an index of interrater agreement between m raters on categorical data.
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
The coefficient described by Fleiss (1971) does not reduce to Cohen's Kappa (unweighted) for m=2 raters. Therefore, the exact Kappa coefficient, which is slightly higher in most cases, was proposed by Conger (1980).\cr
Light's Kappa equals the average of all possible combinations of bivariate Kappas between raters.\cr
The confidence levels can only be reported using Fleiss' formulation of Kappa.
}
\value{
a single numeric value if no confidence intervals are requested,\cr
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}

\references{
Conger, A.J. (1980): Integration and generalisation of Kappas for multiple raters. \emph{Psychological Bulletin}, 88, 322-328

Fleiss, J.L. (1971): Measuring nominal scale agreement among many raters \emph{Psychological Bulletin}, 76, 378-382

Fleiss, J.L., Levin, B., & Paik, M.C. (2003): \emph{Statistical Methods for Rates and Proportions}, 3rd Edition. New York: John Wiley & Sons

Light, R.J. (1971): Measures of response agreement for qualitative data: Some generalizations and alternatives. \emph{Psychological Bulletin}, 76, 365-377.

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
