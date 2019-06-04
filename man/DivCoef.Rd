\name{DivCoef}
\alias{DivCoef}
\title{Rao's Diversity Coefficient }
\description{
Calculates Rao's diversity coefficient (also known as "Quadratic Entropy") within samples.
}
\usage{
DivCoef(df, dis, scale)
}
\arguments{
  \item{df}{a data frame with elements as rows, samples as columns,
  and abundance, presence-absence or frequencies as entries}
  \item{dis}{an object of class \code{dist} containing distances or dissimilarities among elements.
  If \code{dis} is NULL, Gini-Simpson index is performed.}
  \item{scale}{a logical value indicating whether or not the diversity coefficient
  should be scaled by its maximal value over all frequency distributions.}
}
\value{
Returns a data frame with samples as rows and the diversity coefficient within samples as columns
}
\references{
Rao, C.R. (1982) Diversity and dissimilarity coefficients: a unified approach.
\emph{Theoretical Population Biology}, \bold{21}, 24--43.

Gini, C. (1912) Variabilita e mutabilita. \emph{Universite di Cagliari III}, Parte II.

Simpson, E.H. (1949) Measurement of diversity. \emph{Nature}, \bold{163}, 688.

Champely, S. and Chessel, D. (2002) Measuring biological diversity using Euclidean metrics.
\emph{Environmental and Ecological Statistics}, \bold{9}, 167--177.

}
\note{ This function was previously published as \code{divc()} in the  \pkg{ade4} package and has been integrated here without logical changes.
}

\author{Sandrine Pavoine \email{pavoine@biomserv.univ-lyon1.fr}
}
\examples{
# data(ecomor)
# dtaxo <- dist.taxo(ecomor$taxo)
# DivCoef(ecomor$habitat, dtaxo)

# data(humDNAm)
# DivCoef(humDNAm$samples, sqrt(humDNAm$distances))
}
\keyword{multivariate}
