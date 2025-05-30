% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moments.R
\name{mpois}
\alias{mpois}
\title{Mean and Variance of the Poisson Distribution}
\usage{
mpois(lambda)
}
\arguments{
\item{lambda}{Rate parameter (mean)}
}
\value{
List with mean and variance
}
\description{
Formula:
\eqn{\mu = \lambda}
\eqn{\mathrm{Var}(X) = \lambda}
}
\examples{
mpois(lambda = 4)
}
\references{
Ross, S.M. (2014). Introduction to Probability Models. Academic Press.
}
\seealso{
\code{\link[stats]{dpois}}
}
