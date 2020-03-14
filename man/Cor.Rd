% File src/library/stats/man/cor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{Cor}
\title{Covariance and Correlation (Matrices)}
\usage{

Cov(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

Cor(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

}

\alias{Cov}
\alias{Cor}
\description{
  \code{Cov} and \code{Cor} compute the covariance or correlation of \code{x} and \code{y} if these
  are vectors.   If \code{x} and \code{y} are matrices then the
  covariances (or correlations) between the columns of \code{x} and the
  columns of \code{y} are computed.

}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{y}{\code{NULL} (default) or a vector, matrix or data frame with
    compatible dimensions to \code{x}.   The default is equivalent to
    \code{y = x} (but more efficient).}

  \item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.  This must be (an abbreviation of) one of the strings
    \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"},
    \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.}
  \item{method}{a character string indicating which correlation
    coefficient (or covariance) is to be computed.  One of
    \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}:
    can be abbreviated.}

}
\value{For \code{r <- Cor(*, use = "all.obs")}, it is now guaranteed that
  \code{all(abs(r) <= 1)}.
}
\details{
  For \code{Cov} and \code{Cor} one must \emph{either} give a matrix or
  data frame for \code{x} \emph{or} give both \code{x} and \code{y}.

  The inputs must be numeric (as determined by \code{\link{is.numeric}}:
  logical values are also allowed for historical compatibility): the
  \code{"kendall"} and \code{"spearman"} methods make sense for ordered
  inputs but \code{\link{xtfrm}} can be used to find a suitable prior
  transformation to numbers.

  If \code{use} is \code{"everything"}, \code{\link{NA}}s will
  propagate conceptually, i.e., a resulting value will be \code{NA}
  whenever one of its contributing observations is \code{NA}.\cr
  If \code{use} is \code{"all.obs"}, then the presence of missing
  observations will produce an error.  If \code{use} is
  \code{"complete.obs"} then missing values are handled by casewise
  deletion (and if there are no complete cases, that gives an error).
  \cr
  \code{"na.or.complete"} is the same unless there are no complete
  cases, that gives \code{NA}.
  Finally, if \code{use} has the value \code{"pairwise.complete.obs"}
  then the correlation or covariance between each pair of variables is
  computed using all complete pairs of observations on those variables.
  This can result in covariance or correlation matrices which are not positive
  semi-definite, as well as \code{NA} entries if there are no complete
  pairs for that pair of variables.   For \code{Cov} and \code{Var},
  \code{"pairwise.complete.obs"} only works with the \code{"pearson"}
  method.
  Note that (the equivalent of) \code{Var(double(0), use = *)} gives
  \code{NA} for \code{use = "everything"} and \code{"na.or.complete"},
  and gives an error in the other cases.

  The denominator \eqn{n - 1} is used which gives an unbiased estimator
  of the (co)variance for i.i.d. observations.
  These functions return \code{\link{NA}} when there is only one
  observation (whereas S-PLUS has been returning \code{NaN}), and
  fail if \code{x} has length zero.

  For \code{Cor()}, if \code{method} is \code{"kendall"} or
  \code{"spearman"}, Kendall's \eqn{\tau}{tau} or Spearman's
  \eqn{\rho}{rho} statistic is used to estimate a rank-based measure of
  association.  These are more robust and have been recommended if the
  data do not necessarily come from a bivariate normal distribution.\cr
  For \code{Cov()}, a non-Pearson method is unusual but available for
  the sake of completeness.  Note that \code{"spearman"} basically
  computes \code{Cor(R(x), R(y))} (or \code{Cov(., .)}) where \code{R(u)
  := rank(u, na.last = "keep")}. In the case of missing values, the
  ranks are calculated depending on the value of \code{use}, either
  based on complete observations, or based on pairwise completeness with
  reranking for each pair.

  Scaling a covariance matrix into a correlation one can be achieved in
  many ways, mathematically most appealing by multiplication with a
  diagonal matrix from left and right, or more efficiently by using
  \code{\link{sweep}(.., FUN = "/")} twice.
}
\note{
  Some people have noted that the code for Kendall's tau is slow for
  very large datasets (many more than 1000 cases).  It rarely makes
  sense to do such a computation, but see function
  \code{\link[pcaPP:cor.fk]{cor.fk}} in package \CRANpkg{pcaPP}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{


  \code{\link{cor.test}} for confidence intervals (and tests).

  \code{\link{cov.wt}} for \emph{weighted} covariance computation.

  \code{\link{Var}}, \code{\link{SD}} for variance and standard deviation (vectors).
}
\examples{
## Two simple vectors
Cor(1:10, 2:11) # == 1

## Correlation Matrix of Multivariate sample:
(Cl <- Cor(longley))
## Graphical Correlation Matrix:
symnum(Cl) # highly correlated

## Spearman's rho  and  Kendall's tau
symnum(clS <- Cor(longley, method = "spearman"))
symnum(clK <- Cor(longley, method = "kendall"))
## How much do they differ?
i <- lower.tri(Cl)
Cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))


##--- Missing value treatment:
% "everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"
C1 <- Cov(swiss)
range(eigen(C1, only.values = TRUE)$values) # 6.19        1921

## swM := "swiss" with  3 "missing"s :
swM <- swiss
colnames(swM) <- abbreviate(colnames(swiss), min=6)
swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"

## Consider all 5 "use" cases :
(C. <- Cov(swM)) # use="everything"  quite a few NA's in cov.matrix
try(Cov(swM, use = "all")) # Error: missing obs...
C2 <- Cov(swM, use = "complete")
stopifnot(identical(C2, Cov(swM, use = "na.or.complete")))
range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
C3 <- Cov(swM, use = "pairwise")
range(eigen(C3, only.values = TRUE)$values) # 6.19   1938

## Kendall's tau doesn't change much:
symnum(Rc <- Cor(swM, method = "kendall", use = "complete"))
symnum(Rp <- Cor(swM, method = "kendall", use = "pairwise"))
symnum(R. <- Cor(swiss, method = "kendall"))

## "pairwise" is closer componentwise,
summary(abs(c(1 - Rp/R.)))
summary(abs(c(1 - Rc/R.)))

## but "complete" is closer in Eigen space:
EV <- function(m) eigen(m, only.values=TRUE)$values
summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))

}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
