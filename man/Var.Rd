% File src/library/stats/man/cor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{Var}
\alias{Var}
\alias{Var.default}
\alias{Var.Freq}
\alias{SD}


\title{(Weighted) Variance and Standard Deviation}
\usage{

Var(x, \dots)

\method{Var}{Freq}(x, breaks, \dots)

\method{Var}{default}(x, estimator = c("unbiased",  "ML"), weights = NULL, na.rm = FALSE, \dots)

SD(x, estimator = c("unbiased",  "ML"), weights = NULL, na.rm = FALSE, \dots)
}

\description{
  \code{Var()} computes the variance of \code{x}, allowing the definition of weights 
  (unlike base R's \code{\link{var}()} function). Using the estimator \code{ML} 
  returns the uncorrected sample variance (which is a biased estimator for 
  the sample variance). \cr\code{SD} yields the standard deviation following the 
  same logic.
}

\arguments{
  \item{x}{a numeric vector, matrix or data frame.}

  \item{estimator}{determines the estimator type; if \code{"unbiased"} (the default) 
  then the usual unbiased estimate (using \eqn{n - 1} as denominator) is returned, 
  if \code{"ML"} then it is the maximum likelihood estimate for a Gaussian 
  distribution (denominator \eqn{n}). Internally \code{\link{cov.wt}} is used 
  for both methods.}

  \item{weights}{
   non-negative numeric vector of weights the same length as \code{x} 
   nterpreted as frequency (replication) weights. Observations with larger weights contribute
   more strongly to the empirical distribution.
  }
  \item{na.rm}{logical. Should missing values be removed?}

 \item{breaks}{breaks for calculating the variance for classified 
 data as composed by \code{\link{Freq}}.}

  \item{\dots}{further arguments passed to or from other methods.}

}

\value{single numeric value }

\details{
  Using estimator \code{"unbiased"} the denominator \eqn{n - 1} 
  is used (known as "Bessel's correction") which gives an unbiased estimator
  of the (co)variance for i.i.d. observations.\cr
  \code{"ML"} yields the biased version using the denominator \eqn{n}.
  
  These functions return \code{\link{NA}} when there is only one
  observation, and fail if \code{x} has length zero.
  
  \bold{Note:}\verb{    }
  Analytic (precision) weights are not supported. For likelihood-based
weighted variance estimation, see \code{\link[stats]{cov.wt}}.

}


\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{VarCI}} for confidence intervals and \code{\link{VarTest}} for tests.
  
  base R's implementations  \code{\link{var}}, \code{\link{sd}}, \code{\link{cov}}
  
  also consider \code{\link{MAD}}, the most robust alternative
}


\examples{
Var(1:10)                 # 9.166667
SD(1:10)

Var(1:5, weights=1:5)     # 2.5
# same as c(1 2 2 3 3 3 4 4 4 4 5 5 5 5 5):
Var(rep(1:5, times=1:5))  # 2.5

# weighted Variance
set.seed(45)
(z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, replace=TRUE)))))
Var(z, weights=w)
SD(z, weights=w)

# check!
all.equal(Var(x), Var(z, weights=w))


# Variance for frequency tables
Var(Freq(as.table(c(6,16,24,25,17))),
     breaks=c(0, 10, 20, 30, 40, 50))
     
}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
