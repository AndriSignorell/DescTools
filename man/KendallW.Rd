% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KendallW.R
\name{KendallW}
\alias{KendallW}
\title{Kendall's Coefficient of Concordance W}
\usage{
KendallW(x, correct = FALSE, test = FALSE, na.rm = NULL)
}
\arguments{
\item{x}{\eqn{n \times m}{k x m} matrix or dataframe, k subjects (in rows) m
raters (in columns).}

\item{correct}{a logical indicating whether the coefficient should be
corrected for ties within raters. (Default \code{FALSE})}

\item{test}{a logical indicating whether the test statistic and p-value
should be reported. (Default \code{FALSE})}

\item{na.rm}{is not longer supported and will be ignored.}
}
\value{
Either a single value if \code{test = FALSE} or else \cr

a list with class \dQuote{\code{htest}} containing the following components:
\item{statistic}{the value of the chi-square statistic.} \item{p.value }{the
p-value for the test.} \item{method}{the character string \dQuote{Kendall's
coefficient of concordance W}.} \item{data.name}{a character string giving
the name(s) of the data.} \item{estimate}{the coefficient of concordance.}
\item{parameter}{the degrees of freedom df, the number of subjects examined
and the number of raters.}
}
\description{
Computes Kendall's coefficient of concordance, a popular measure of
association. It is an index of interrater reliability of ordinal data. The
coefficient could be corrected for ties within raters.
}
\details{
The test for Kendall's W is completely equivalent to
\code{\link[stats]{friedman.test}}. The only advantage of this test over
Friedman's is that Kendall's W has an interpretation as the coefficient of
concordance. The test itself is only valid for large samples.\cr Kendall's W
should be corrected for ties, if raters did not use a true ranking order for
the subjects.
The function warns if ties are present and no correction has been required.

In the presence of \code{NAs} the algorithm is switched to a generalized form for
randomly incomplete datasets introduced in Brueckl (2011).
This approach uses the mean Spearman \eqn{\rho}{rho} of all pairwise comparisons
(see Kendall, 1962):\cr

\deqn{W = (1+mean(\rho)*(k-1)) / k}

where k is the mean number of (pairwise) ratings per object and mean(\eqn{\rho}{rho}) is
calculated weighted, according to Taylor (1987), since the pairwise are
possibly based on a different number of ratings, what must be reflected
in weights.
In case of complete datasets, it yields the same results
as usual implementations of Kendall's W, except for tied ranks. In case
of tied ranks, the (pairwise) correction of s used, which (already with
complete datasets) results in slightly different values than the tie
correction explicitly specified for W.
}
\examples{

anxiety <- data.frame(rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
                      rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
                      rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2))

KendallW(anxiety, TRUE)

# with test results
KendallW(anxiety, TRUE, test=TRUE)

# example from Siegel and Castellan (1988)
d.att <- data.frame(
  id        = c(4,21,11),
  airfare   = c(5,1,4),
  climate   = c(6,7,5),
  season    = c(7,6,1),
  people    = c(1,2,3),
  program   = c(2,3,2),
  publicity = c(4,5,7),
  present   = c(3,4,6),
  interest  = c(8,8,8)
)

KendallW(t(d.att[, -1]), test = TRUE)

# which is perfectly the same as
friedman.test(y=as.matrix(d.att[,-1]), groups = d.att$id)
}
\references{
Kendall, M.G. (1948) \emph{Rank correlation methods}. London:
Griffin.

Kendall, M.G. (1962). Rank correlation methods (3rd ed.). London: Griffin.

Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der
Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der
Stimme und Sprechweise von Frauen, Berlin: Logos, 88–103.

Taylor, J.M.G. (1987). Kendall's and Spearman's correlation coefficients in the presence of a blocking variable. \emph{Biometrics}, 43, 409–416.
}
\seealso{
\code{\link[stats]{cor}}, \code{\link{KappaM}},
\code{\link{CronbachAlpha}}, \code{\link{ICC}},
\code{\link[stats]{friedman.test}}
}
\author{
Andri Signorell \href{mailto:andri@signorell.net}{andri@signorell.net}\cr
based on code by Matthias Gamer \href{mailto:m.gamer@uke.uni-hamburg.de}{m.gamer@uke.uni-hamburg.de}\cr
and Markus Brueckl \href{mailto:markus.brueckl@tu-berlin.de}{markus.brueckl@tu-berlin.de}
}
\keyword{multivar}
