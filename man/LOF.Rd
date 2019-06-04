\name{LOF}
\alias{LOF}
\title{ Local Outlier Factor }
\description{ A function that finds the local outlier factor (Breunig et al.,2000) of
the matrix "data" using k neighbours. The local outlier factor (LOF) is a measure of outlyingness
that is calculated for each observation. The user decides whether or not an observation 
will be considered an outlier based on this measure. The LOF takes into consideration
the density of the neighborhood around the observation to determine its outlyingness.
  }
\usage{
LOF(data, k)
}
\arguments{
  \item{data}{ The data set to be explored}
  \item{k}{ The kth-distance to be used to calculate the LOF's.}
}
\details{
  The LOFs are calculated over a range of values, and the max local outlier factor
is determined over this range.
}
\note{This function was originally published in the library dprep.}
\value{
  \item{lof}{ A vector with the local outlier factor of each observation}
}
\references{ Breuning, M., Kriegel, H., Ng, R.T, and Sander. J. (2000). 
LOF: Identifying density-based local outliers. \emph{In Proceedings of the ACM SIGMOD 
International Conference on Management of Data}}

\author{Caroline Rodriguez}
\examples{
# Detecting the top 10 outliers using the LOF algorithm

(iris.lof <- LOF(iris[,-5], 10))
}

\keyword{methods}
