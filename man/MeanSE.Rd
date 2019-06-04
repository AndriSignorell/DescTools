\name{MeanSE}
\alias{MeanSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standard Error of Mean
%%  ~~function to do ... ~~
}
\description{
Calculates the standard error of mean.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MeanSE(x, sd = NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
\item{sd}{the standard deviation of \code{x}. If provided it's interpreted as sd of the population. If left to \code{NULL} (default) the sample \code{sd(x)} will be used.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{MeanSE calculates the standard error of the mean defined as: \deqn{\frac{\sigma}{\sqrt{n}}}
\eqn{\sigma} being standard deviation of \code{x} and n the length of \code{x}.
}

\value{the standard error as numeric value.
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{MeanCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(d.pizza)

MeanSE(d.pizza$price, na.rm=TRUE)

# evaluate data.frame
sapply(d.pizza[,1:4], MeanSE, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

