\name{ImputeKnn}
\alias{ImputeKnn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill in NA values with the values of the nearest neighbours
}
\description{
Function that fills in all NA values using the k Nearest
Neighbours of each case with NA values.
By default it uses the values of the neighbours and 
obtains an weighted (by the distance to the case) average
of their values to fill in the unknows.
If meth='median' it uses the median/most frequent value,
instead.
}
\usage{
ImputeKnn(data, k = 10, scale = TRUE, meth = "weighAvg", distData = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with the data set
}
  \item{k}{
The number of nearest neighbours to use (defaults to 10)
}
  \item{scale}{
Boolean setting if the data should be scale before finding the nearest neighbours (defaults
to TRUE)
}
  \item{meth}{
String indicating the method used to calculate the value to fill in each
NA. Available values are 'median' or 'weighAvg' (the default).
}
\item{distData}{
  Optionally you may sepecify here a data frame containing the data set
  that should be used to find the neighbours. This is usefull when
  filling in NA values on a test set, where you should use only
  information from the training set. This defaults to NULL, which means
  that the neighbours will be searched in \code{data}
}
}
\details{
This function uses the k-nearest neighbours to fill in the unknown (NA)
values in a data set. For each case with any NA value it will search for
its k most similar cases and use the values of these cases to fill in
the unknowns.

If \code{meth='median'}  the function will use either the median (in
case of numeric variables) or the most frequent value (in case of
factors), of the neighbours to fill in the NAs. If
\code{meth='weighAvg'} the function will use a weighted average of the
values of the neighbours. The weights are given by \code{exp(-dist(k,x)}
where \code{dist(k,x)} is the euclidean distance between the case with
NAs (x) and the neighbour k.
}
\value{
  A data frame without NA values
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{complete.cases}}, \code{\link{na.omit}}
}
\examples{
cleanPizza <- ImputeKnn(d.pizza[, -2])   # no dates allowed
summary(cleanPizza)
}
\keyword{models}
