% File src/library/stats/man/cor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{Var}
\alias{Var}
\alias{Var.default}
\alias{Var.Freq}

\title{Variance}
\usage{

Var(x, \dots)

\method{Var}{Freq}(x, breaks, \dots)

\method{Var}{default}(x, weights = NULL, na.rm = FALSE, method = c("unbiased",  "ML"), \dots)
}

\description{
  \code{Var()} computes the variance of \code{x}.  If \code{x} is a matrix variances of the columns of \code{x} are computed.
}

\arguments{
  \item{x}{a numeric vector, matrix or data frame.}

  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{na.rm}{logical. Should missing values be removed?}

  \item{method}{determines the estimator type; if \code{"unbiased"} (the default) then the usual unbiased estimate (using Bessel's correction) is returned, if \code{"ML"} then it is the maximum likelihood estimate for a Gaussian distribution. Uses stats:cov.wt for both methods.}
 \item{breaks}{breaks for calculating the variance for classified data as composed by \code{\link{Freq}}.}

  \item{\dots}{further arguments passed to or from other methods.}

}
\value{For \code{r <- Cor(*, use = "all.obs")}, it is now guaranteed that
  \code{all(abs(r) <= 1)}.
}
\details{
  \code{Var} is just another interface to \code{Cov}.
  
  The denominator \eqn{n - 1} is used which gives an unbiased estimator
  of the (co)variance for i.i.d. observations.
  These functions return \code{\link{NA}} when there is only one
  observation (whereas S-PLUS has been returning \code{NaN}), and
  fail if \code{x} has length zero.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{

  \code{\link{cor}}, \code{\link{cov}} for covariance and correlation matrices
  
  \code{\link{cor.test}} for confidence intervals (and tests).

  \code{\link{cov.wt}} for \emph{weighted} covariance computation.

  \code{\link{sd}} for standard deviation (vectors).
}
\examples{
Var(1:10)  # 9.166667

Var(1:5, 1:5) # 2.5

# weighted Variance
set.seed(45)
(z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, replace=TRUE)))))
Var(z, w=w)

# check!
all.equal(Var(x), Var(z, w=w))


# Variance for frequency tables
Var(Freq(as.table(c(6,16,24,25,17))),
     breaks=c(0, 10, 20, 30, 40, 50))
     
}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
