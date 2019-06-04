\name{BoxCoxLambda}
\alias{BoxCoxLambda}
\title{Automatic Selection of Box Cox Transformation Parameter}
\usage{BoxCoxLambda(x, method = c("guerrero", "loglik"), lower = -1, upper = 2)
}
\arguments{
\item{x}{a numeric vector or time series}
\item{method}{method to be used in calculating lambda. Can be either "guerrero" (default) or "loglik".}
\item{lower}{lower limit for possible lambda values, default is -1.}
\item{upper}{upper limit for possible lambda values, default is 2.}
}
\description{An automatic selection of the Box Cox transformation parameter is estimated with two methods.\cr 
Guerrero's (1993) method yields a lambda which minimizes the coefficient of variation for subseries of \code{x}. 
For method \code{"loglik"}, the value of lambda is chosen to maximize the profile log likelihood of a linear model fitted to \code{x}. 
For non-seasonal data, a linear time trend is fitted while for seasonal data, a linear time trend with seasonal dummy variables is used.
}
\value{a number indicating the Box-Cox transformation parameter.
}

\note{ This function was previously published as \code{BoxCox.lambda()} in the  \pkg{forecast} package and has been integrated here without logical changes. 
}
\references{Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26} 211--246.

Guerrero, V.M. (1993) Time-series analysis supported by power transformations. \emph{Journal of Forecasting}, \bold{12}, 37--48.
}
\seealso{\code{\link{BoxCox}}}

\author{Leanne Chhay and Rob J Hyndman}
\examples{

lambda <- BoxCoxLambda(AirPassengers, lower=0)

}
\keyword{ts}
