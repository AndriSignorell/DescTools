% File src/library/stats/man/sd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{SD}
\alias{SD}
\title{(Weighted) Standard Deviation}
\description{
  This function computes the standard deviation of the values in
  \code{x}.
  If \code{na.rm} is \code{TRUE} then missing values are removed before
  computation proceeds.
}
\usage{
SD(x, weights = NULL, na.rm = FALSE, ...)
}
\arguments{
  \item{x}{a numeric vector or an \R object which is coercible to one
    by \code{as.double(x)}.}

  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}. }

  \item{na.rm}{logical.  Should missing values be removed?}

  \item{\dots}{further arguments passed to or from other methods.}

}
\details{
  Like \code{\link{var}} this uses denominator \eqn{n - 1}.

  The standard deviation of a zero-length vector (after removal of
  \code{NA}s if \code{na.rm = TRUE}) is not defined and gives an error.
  The standard deviation of a length-one vector is \code{NA}.
}
\seealso{
  \code{\link{var}} for its square, and \code{\link{mad}}, the most
  robust alternative.
}
\examples{
SD(1:2)^2
}
\keyword{univar}
