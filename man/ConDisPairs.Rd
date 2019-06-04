\name{ConDisPairs}
\alias{ConDisPairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Concordant and Discordant Pairs
%%  ~~function to do ... ~~
}
\description{This function counts concordant and discordant pairs for two variables x, y with at least ordinal scale, aggregated in a
2way table. This is the base for many association measures like Goodman Kruskal's gamma, but also all tau measures.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ConDisPairs(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 2-dimensional table. The column and the row order must be the logical one.
%%     ~~Describe \code{x} here~~
}
}
\details{
The code is so far implemented in R (O(n^2)) and therefore slow for large sample sizes (>5000).
An O(n log(n)) implementation is on track.
}
\value{a list with the number of concordant pairs, the number of discordant pairs and
the matrix

}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons,
pp. 57-59.

Goodman, L. A., & Kruskal, W. H. (1954) Measures of
association for cross classifications. \emph{Journal of the
American Statistical Association}, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310-364.

%% ~put references to the literature/web site here ~
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{
Association measures: \cr
\code{\link{KendallTauA}} (tau-a), \code{\link{cor}} (method="kendall") for tau-b, \code{\link{StuartTauC}} (tau-c), \code{\link{SomersDelta}}\cr
\code{\link{Lambda}}, \code{\link{GoodmanKruskalTau}} (tau), \code{\link{UncertCoef}}, \code{\link{MutInf}}

}
\examples{
tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
ConDisPairs(tab)
}
\keyword{ multivariate}
