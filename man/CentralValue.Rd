\name{CentralValue}
\alias{CentralValue}

\title{
Obtain statistic of centrality
}
\description{
  This function obtains a statistic of centrality of a variable given a
  sample of its values.
}
\usage{
CentralValue(x, ws = NULL)
}

\arguments{
  \item{x}{
A vector of values (the sample).
}
  \item{ws}{
A vector of case weights (defaulting to NULL, i.e. no case weights).
}
}
\details{
  If the variable is numeric it returns de median of the given sample, if it
 is a factor it returns the mode. In other cases it
 tries to convert to a factor and then returns the mode.

}
\value{
  A number if the variable is numeric. A string with the name of the
  most frequent nominal value, otherwise.
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{mean}}, \code{\link{median}} 
}
\examples{
# An example with numerical data
x <- rnorm(100)
CentralValue(x)
# An example with nominal data
y <-
factor(sample(1:10,200,replace=TRUE),levels=1:10,labels=paste('v',1:10,sep=''))
CentralValue(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
