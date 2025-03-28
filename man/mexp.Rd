% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moments.R
\name{mexp}
\alias{mexp}
\title{Mean and Variance of the Exponential Distribution}
\usage{
mexp(rate)
}
\arguments{
\item{rate}{Rate parameter (1 / mean)}
}
\value{
List with mean and variance
}
\description{
Formula:
\eqn{\mu = \frac{1}{\lambda}}
\eqn{\mathrm{Var}(X) = \frac{1}{\lambda^2}}
}
\examples{
mexp(rate = 0.5)
}
\seealso{
\code{\link[stats]{dexp}}
}
