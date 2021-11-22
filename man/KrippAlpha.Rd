\name{KrippAlpha}
\alias{KrippAlpha}
\title{Krippendorff's Alpha Reliability Coefficient}
\description{
 Calculate the alpha coefficient of reliability proposed by Krippendorff.
}
\usage{
 KrippAlpha(x, method=c("nominal", "ordinal", "interval", "ratio"))
}
\arguments{
  \item{x}{classifier x object matrix of classifications or scores}
  \item{method}{data level of x}
}
\value{
 A list with class '"irrlist"' containing the following components: 
 \item{method}{a character string describing the method.}
 \item{subjects}{the number of data objects.}
 \item{raters}{the number of raters.}
 \item{irr.name}{a character string specifying the name of the coefficient.}
 \item{value}{value of alpha.}
 \item{stat.name}{here "nil" as there is no test statistic.}
 \item{statistic}{the value of the test statistic (NULL).}
 \item{p.value}{the probability of the test statistic (NULL).}
 \item{cm}{the concordance/discordance matrix used in the calculation of alpha}
 \item{data.values}{a character vector of the unique data values}
 \item{levx}{the unique values of the ratings}
 \item{nmatchval}{the count of matches, used in calculation}
 \item{data.level}{the data level of the ratings ("nominal","ordinal",
  "interval","ratio")}
}
\references{
 Krippendorff, K. (1980) \emph{Content analysis: An introduction to its methodology}. Beverly Hills, CA: Sage.
}
\author{Jim Lemon <jim@bitwrit.com.au>}

\note{Krippendorff's alpha coefficient is particularly useful where
 the level of measurement of classification data is higher than nominal
 or ordinal.
 https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-016-0200-9
 }
\note{
This function was previously published as \code{kripp.alpha()} in the  \pkg{irr} package and has been
integrated here without logical changes, but with some adaptations in the result structure.
}

\seealso{ \code{\link{CronbachAlpha}}, \code{\link{KappaM}}, \code{\link{CohenKappa}}}

\examples{# the "C" data from Krippendorff
 nmm <- matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
                 1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA), nrow=4)
 
 # first assume the default nominal classification
 KrippAlpha(nmm)
 
 # now use the same data with the other three methods
 KrippAlpha(nmm, "ordinal")
 KrippAlpha(nmm, "interval")
 KrippAlpha(nmm, "ratio") 
}
\keyword{misc}
