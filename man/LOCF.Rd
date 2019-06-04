\name{LOCF}
\alias{LOCF}
\alias{LOCF.default}
\alias{LOCF.matrix}
\alias{LOCF.data.frame}
\docType{data}
\title{Last Observation Carried Forward
%%   ~~ data name/kind ... ~~
}
\description{In longitudinal studies it's common that individuals drop out before all responses can be obtained. Measurements obtained before the individual dropped out can be used to impute the unknown measurement(s). The last observation carried forward method is one way to impute values for the missing observations. For the last observation carried forward (LOCF) approach the missing values are replaced by the last observed value of
that variable for each individual regardless of when it occurred.

\code{LOCF()} replaces \code{NA}s with the most recent non-NA prior to it.
}
\usage{
LOCF(x)

\method{LOCF}{default}(x)
\method{LOCF}{data.frame}(x)
\method{LOCF}{matrix}(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, a data.frame or a matrix containing NAs.
%%     ~~Describe \code{x} here~~
}
}
\details{The function will replace all NAs found in a vector with the last earlier value not being NA.
In data.frames each column will be treated as described.

It should be noted, that the last observation carried forward approach may result in biased estimates and may underestimate
the variability.
}
\value{a vector with the same dimension as x.
}
\author{Daniel Wollschlaeger <dwoll@psychologie.uni-kiel.de>}
\seealso{See also the package \pkg{Hmisc} for less coarse imputation functions.}
\examples{
d.frm <- data.frame(
  tag=rep(c("mo", "di", "mi", "do", "fr", "sa", "so"), 4)
, val=rep(c(runif(5), rep(NA,2)), 4) )

d.frm$locf <- LOCF( d.frm$val )
d.frm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
