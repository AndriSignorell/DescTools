% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moments.R
\name{mchisq}
\alias{mchisq}
\title{Mean and Variance of the Chi-Squared Distribution}
\usage{
mchisq(df)
}
\arguments{
\item{df}{Degrees of freedom}
}
\value{
List with mean and variance
}
\description{
Formula:
\eqn{\mu = df}
\eqn{\mathrm{Var}(X) = 2 \cdot df}
}
\examples{
mchisq(df = 4)
}
\seealso{
\code{\link[stats]{dchisq}}
}
