\name{MeanAD}
\alias{MeanAD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mean Absolute Deviation From a Center Point
%%  ~~function to do ... ~~
}
\description{Calculates the mean absolute deviation from a center point, typically the sample mean or the median.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MeanAD(x, FUN = mean, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector containing the observations.
%%     ~~Describe \code{x} here~~
}
  \item{FUN}{the name of a function to be used as center. Can as well be a self defined function.
  Default is mean.

%%     ~~Describe \code{na.rm} here~~
}
  \item{na.rm}{a logical value indicating whether or not missing values should be removed. Defaults to FALSE.

%%     ~~Describe \code{na.rm} here~~
}
}
\details{
The MeanAD function calculates the mean absolute deviation from the mean value (or from another
supplied center point) of
x, after having removed \code{NA} values (if requested):
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left | x_{i}-c \right |  \; \; \; \textup{where} \; c=mean(x) \; \textup{or} \; c=med(x)}{1/n sum(|x_i - c|) where c=mean(x) or c=med(x)}
\cr
It exists primarily to simplify the discussion of
descriptive statistics during an introductory stats class.

%%  ~~ If necessary, more details than the description above ~~
}
\value{Numeric value.}

\author{Andri Signorell <andri@signorell.net> following an idea of Daniel Navarro (\code{aad} in the \pkg{lsr} package)

}

\seealso{\code{\link{mad}}
}
\examples{
x <- runif(100)
MeanAD(x)

speed <- c(58, 88, 40, 60, 72, 66, 80, 48, NA)
MeanAD(speed)
MeanAD(speed, na.rm=TRUE)


# using the median as centerpoint
x <- c(2,3,5,3,1,15,23)

MeanAD(x, FUN=mean)
MeanAD(x, FUN=median)

# define a fix center
MeanAD(x, 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

