\name{Measures of Shape}
\alias{Skew}
\alias{Kurt}
\title{
Skewness and Kurtosis
}
\description{
\code{Skew} computes the skewness, \code{Kurt} the excess kurtosis of the values in x. 

}
\usage{

Skew(x, conf.level = NA, sides=c("two.sided", "left", "right"), 
     method = c("boot", "classic"), 
     estimator = 3, weights=NULL, na.rm = FALSE, ...)

Kurt(x, conf.level = NA, sides=c("two.sided", "left", "right"), 
     method = c("boot", "classic"), 
     estimator = 3, weights=NULL, na.rm = FALSE, ...)

}
\arguments{
  \item{x}{
  a numeric vector. An object which is not a vector is coerced (if possible) by \code{as.vector}.
}
\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated. }

\item{sides}{a character string specifying the side of the confidence
interval, must be one of \code{"two.sided"} (default), \code{"left"} or
\code{"right"}. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}. You can specify just the initial
letter.}

\item{method}{a character string, defining the type of
intervals required. The value should be one out of 
\code{"classic"}, \code{"boot"} (default).
Further arguments for the boot function (as \code{R}, \code{type}, ...) can 
be provided by the dots' argument if needed.
}  

\item{estimator}{
  integer, either 1, 2 or 3 (default) defining the algorithm used for calculation. See Details.
}

  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
}

\item{\dots}{ the dots are passed to the function \code{\link[boot]{boot}}, when confidence intervalls are calculated.}

}

\details{\code{Kurt()} returns the excess kurtosis, therefore the kurtosis calculates as \code{Kurt(x) + 3} if required.

If \code{na.rm} is \code{TRUE} then missing values are removed before computation proceeds. \cr

The estimator for calculating the skewness can either be:\cr
  \code{1:   g_1 = m_3 / m_2^(3/2) } \cr
  \code{2:   G_1 = g_1 * sqrt(n(n-1)) / (n-2) }\cr
  \code{3:   b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) } \cr

and the one for the kurtosis:\cr
  \code{1:  g_2 = m_4 / m_2^2 - 3 }  \cr
  \code{2:  G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) }  \cr
  \code{3:  b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 } \cr

  1 is the typical definition used in Stata and in many older textbooks.   \cr
  2 is used in SAS and SPSS.  \cr
  3 is used in MINITAB and BMDP. \cr

Cramer (1997) mentions the asymptotic standard error of the skewness, 
resp. kurtosis: \cr
\preformatted{ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )
ASE.kurt = sqrt((24*n*(n - 1)^2) / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))} to 
be used for calculating the confidence intervals. 
This is implemented here with \code{method="classic"}. \cr However, Joanes and 
Gill (1998) advise against this approach, pointing out that the normal 
assumptions would virtually always be violated. 
They suggest using the bootstrap method. That's why the default method 
for the confidence interval type is set to \code{"boot"}. If not further specified
the boot ci type will be chosen as \code{"bca"}.\cr  

This implementation of the two functions is comparably fast, as the expensive sums are coded in C.

}

\value{
If \code{conf.level} is set to \code{NA} then the result will be  
\item{a}{ single numeric value} and 
if a \code{conf.level} is provided, a named numeric vector with 3 elements:
  \item{est}{the specific estimate, either skewness or kurtosis}
  \item{lci}{lower bound of the confidence interval}
  \item{uci}{upper bound of the confidence interval}
}

\author{Andri Signorell <andri@signorell.net>, David Meyer <david.meyer@r-project.org> (method = 3) }

\references{
Cramer, D. (1997): \emph{Basic Statistics for Social Research} Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and kurtosis. \emph{The Statistician}, 47, 183-189.
}

\seealso{
\code{\link{mean}}, \code{\link{sd}}, similar code in \code{library(e1071)}
}
\examples{
Skew(d.pizza$price, na.rm=TRUE)
Kurt(d.pizza$price, na.rm=TRUE)

# use sapply to calculate skewness for a data.frame
sapply(d.pizza[,c("temperature","price","delivery_min")], Skew, na.rm=TRUE)

# or apply to do that columnwise with a matrix
apply(as.matrix(d.pizza[,c("temperature","price","delivery_min")]), 2, Skew, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math}
