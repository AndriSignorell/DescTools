\name{Outlier}
\alias{Outlier}
\title{Outlier}

\description{Return outliers following Tukey's boxplot and Hampel's median/mad definition.}


\usage{
Outlier(x, method = c("boxplot", "hampel"), value = TRUE,na.rm = FALSE)
}

\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{method}{the method to be used. So far Tukey's boxplot and Hampel's rule are implemented.}
  
  \item{value}{logical. If \code{FALSE}, a vector containing the (integer) indices of the outliers is returned, and if \code{TRUE} (default), a vector containing the matching elements themselves is returned.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
}

\details{Outlier detection is a tricky problem and should be handled with care. We implement Tukey's boxplot rule as a rough idea of spotting extreme values.

Hampel considers values outside of median +/- 3 * (median absolute deviation) to be outliers.}

\value{the values of x lying outside the whiskers in a boxplot \cr
or the indices of them }

\author{Andri Signorell <andri@signorell.net>}

\references{Hampel F. R. (1974) The influence curve and its role in robust estimation, \emph{Journal of the American Statistical Association}, 69, 382-393}

\seealso{\code{\link{boxplot}} }

\examples{
Outlier(d.pizza$temperature, na.rm=TRUE)

# it's the same as the result from boxplot
sort(d.pizza$temperature[Outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE)])
b <- boxplot(d.pizza$temperature, plot=FALSE)
sort(b$out)

# nice to find the corresponding rows
d.pizza[Outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE), ]

# compare to Hampel's rule
Outlier(d.pizza$temperature, method="hampel", na.rm=TRUE)


# outliers for the each driver
tapply(d.pizza$temperature, d.pizza$driver, Outlier, na.rm=TRUE)

# the same as:
boxplot(temperature ~ driver, d.pizza)$out
}

\keyword{ univar }
