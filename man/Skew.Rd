\name{Measures of Shape}
\alias{Skew}
%%\alias{Skew.default}
%%\alias{Skew.matrix}
%%\alias{Skew.data.frame}
\alias{Kurt}
%%\alias{Kurt.default}
%%\alias{Kurt.matrix}
%%\alias{Kurt.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Skewness and Kurtosis
%%  ~~function to do ... ~~
}
\description{
\code{Skew} computes the skewness, \code{Kurt} the excess kurtosis of the values in x. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

Skew(x, weights = NULL, na.rm = FALSE, method = 3, conf.level = NA, 
     ci.type = "bca", R = 1000, ...)

Kurt(x, weights = NULL, na.rm = FALSE, method = 3, conf.level = NA, 
     ci.type = "bca", R = 1000, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a numeric vector. An object which is not a vector is coerced (if possible) by \code{as.vector}.
%%     ~~Describe \code{x} here~~
}
  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
  %%     ~~Describe \code{na.rm} here~~
}
  \item{method}{
  integer out of 1, 2 or 3 (default). See Details.
  %%     ~~Describe \code{na.rm} here~~
}
 \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated. 
%%     ~~Describe \code{conf.level} here~~
}
  \item{ci.type}{The type of confidence interval required. The value should be any subset 
 of the values \code{"classic"}, \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"} or \code{"bca"}  (\code{"all"} 
 which would compute all five types of intervals, is not supported). 
}
\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, 
 some resamples may use one set of weights and others use a different set of weights. In this case \code{R} would be a vector 
 of integers where each component gives the number of resamples from each of the rows of weights.
}
\item{\dots}{ the dots are passed to the function \code{\link{boot}}, when confidence intervalls are calculated.}

}

\details{\code{Kurt()} returns the excess kurtosis, therefore the kurtosis calculates as \code{Kurt(x) + 3} if required.

If \code{na.rm} is \code{TRUE} then missing values are removed before computation proceeds. \cr

The methods for calculating the skewness can either be:\cr
  \code{method = 1:   g_1 = m_3 / m_2^(3/2) } \cr
  \code{method = 2:   G_1 = g_1 * sqrt(n(n-1)) / (n-2) }\cr
  \code{method = 3:   b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) } \cr

and the ones for the kurtosis:\cr
  \code{method = 1:  g_2 = m_4 / m_2^2 - 3 }  \cr
  \code{method = 2:  G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) }  \cr
  \code{method = 3:  b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 } \cr

  method = 1 is the typical definition used in many older textbooks.   \cr
  method = 2 is used in SAS and SPSS.  \cr
  method = 3 is used in MINITAB and BMDP. \cr

Cramer et al. (1997) mention the asymptotic standard error of the skewness, resp. kurtosis: \cr
\preformatted{ASE.skew = sqrt( 6n(n-1)/((n-2)(n+1)(n+3)) )
ASE.kurt = sqrt( (n^2 - 1)/((n-3)(n+5)) )} to be used for calculating the confidence intervals. This is implemented here with \code{ci.type="classic"}. However, Joanes and Gill (1998) advise against this approach, pointing out that the normal assumptions would virtually always be violated. 
They suggest using the bootstrap method. That's why the default method for the confidence interval type is set to \code{"bca"}.\cr  

This implementation of the two functions is comparably fast, as the expensive sums are coded in C.

}

\value{
If \code{conf.level} is set to \code{NA} then the result will be  
\item{}{a single numeric value} and 
if a \code{conf.level} is provided, a named numeric vector with 3 elements:
  \item{skew, kurt}{the specific estimate, either skewness or kurtosis}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\author{Andri Signorell <andri@signorell.net>, David Meyer <david.meyer@r-project.org> (method = 3) }

\references{
Cramer, D. (1997): \emph{Basic Statistics for Social Research} Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and Kurt. \emph{The Statistician}, 47, 183-189.
}

\seealso{
\code{\link{mean}}, \code{\link{sd}}, similar code in \code{library(e1071)}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
