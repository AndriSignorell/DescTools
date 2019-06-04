\name{ICC}
\alias{ICC}
\alias{print.ICC}

\title{ Intraclass Correlations (ICC1, ICC2, ICC3 From Shrout and Fleiss) }
\description{The Intraclass correlation is used as a measure of association when studying the reliability of raters.  Shrout and Fleiss (1979) outline 6 different estimates, that depend upon the particular experimental design. All are implemented and given confidence limits.
}
\usage{
ICC(x, type = c("all", "ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k", "ICC3k"),
    conf.level = NA, na.rm = FALSE)

\method{print}{ICC}(x, digits = 3, ...)

}

\arguments{

\item{x}{\eqn{n \times m}{k x m} matrix or dataframe, k subjects (in rows) m raters (in columns).}
\item{type}{one out of "all", "ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k", "ICC3k". See details.}

\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence intervals will be calculated.}
\item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. If set to \code{TRUE} only      the complete cases of the ratings will be used. Defaults to \code{FALSE}. }
\item{digits}{number of digits to use in printing}
\item{\dots}{further arguments to be passed to or from methods.}
}

\details{Shrout and Fleiss (1979) consider six cases of reliability of ratings done by k raters on n targets.

\tabular{ll}{
ICC1 \tab Each  target is rated by a different  judge and the judges are selected at random.\cr
\tab (This is a one-way ANOVA fixed effects model and is found by  (MSB- MSW)/(MSB+ (nr-1)*MSW)) \cr

ICC2 \tab A random sample of k judges rate each target.  The measure is one of absolute agreement \cr
\tab in the ratings. Found as (MSB- MSE)/(MSB + (nr-1)*MSE + nr*(MSJ-MSE)/nc) \cr
ICC3 \tab A fixed set of k judges rate each target. There is no generalization to a larger population \cr
\tab of judges. (MSB - MSE)/(MSB+ (nr-1)*MSE) \cr
}

Then, for each of these cases, is reliability to be estimated for a single rating or for the average of k ratings?  (The 1 rating case is equivalent to the average intercorrelation, the k rating case to the Spearman Brown adjusted reliability.)

ICC1 is sensitive to differences in means between raters and is a measure of absolute agreement.

ICC2 and ICC3 remove mean differences between judges, but are sensitive to interactions of raters by judges.  \cr
The difference between ICC2 and ICC3 is whether raters are seen as fixed or random effects.

ICC1k, ICC2k, ICC3K reflect the means of k raters.

The intraclass correlation is used if raters are all of the same ``class".  That is, there is no logical way of distinguishing them.  Examples include correlations between  pairs of twins, correlations between raters.  If the variables are logically distinguishable (e.g., different items on a test), then the more typical coefficient is based upon the inter-class correlation (e.g., a Pearson r) and a statistic such as alpha or omega might be used.
}
\value{
if method is set to "all", then the result will be

\item{results}{A matrix of 6 rows and 8 columns, including the ICCs, F test, p values, and confidence limits}
  \item{summary}{The anova summary table}
  \item{stats}{The anova statistics}
  \item{MSW}{Mean Square Within based upon the anova}

if a specific type has been defined, the function will first check, whether no confidence intervals are requested:
if so, the result will be the estimate as numeric value\cr\cr
else a named numeric vector with 3 elements
\item{ICCx}{estimate (name is the selected type of coefficient)}
\item{lwr.ci}{lower confidence interval}
\item{upr.ci}{upper confidence interval}

}
\references{ Shrout, P. E., Fleiss, J. L. (1979) Intraclass correlations: uses in assessing rater reliability. \emph{ Psychological Bulletin}, 86, 420-3428.

McGraw, K. O., Wong, S. P. (1996) Forming inferences about some intraclass correlation coefficients.  \emph{ Psychological Methods}, 1, 30-46. + errata on page 390.

Revelle, W. (in prep) \emph{ An introduction to psychometric theory with applications in R}  Springer. (working draft available at  \url{http://personality-project.org/r/book/}}

\author{William Revelle <revelle@northwestern.edu>, some editorial amendments Andri Signorell <andri@signorell.net>}

\note{The results for the lower and upper Bounds for ICC(2,k) do not match those of SPSS 9 or 10, but do match the definitions of Shrout and Fleiss.  SPSS  seems to have been using the  formula in McGraw and Wong, but not the errata on p 390.  They seem to have fixed it in more recent releases (15). }

\examples{
sf <- matrix(c(
      9, 2, 5, 8,
      6, 1, 3, 2,
      8, 4, 6, 8,
      7, 1, 2, 6,
      10,5, 6, 9,
      6, 2, 4, 7),
      ncol=4, byrow=TRUE,
      dimnames=list(paste("S", 1:6, sep=""), paste("J", 1:4, sep=""))
)

sf  #example from Shrout and Fleiss (1979)
ICC(sf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }

