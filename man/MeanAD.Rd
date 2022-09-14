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
MeanAD(x, weights = NULL, center = Mean, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector containing the observations.
%%     ~~Describe \code{x} here~~
}
  \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{center}{a single numerical value or the name of a function to be used as center. Can as well be a self defined function.
  Default is \code{\link{Mean}()}.

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
The function supports the use of weights. The default function for the center value \code{\link{Mean}()} has a weights arguments, too. If a user defined function is used it must be assured that it has a weights argument.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Numeric value.}

\author{Andri Signorell <andri@signorell.net> following an idea of Danielle Navarro (\code{aad} in the \pkg{lsr} package)

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

MeanAD(x, center=mean)
MeanAD(x, center=median)

# define a fixed center
MeanAD(x, center=4)

# use of weights
MeanAD(x=0:6, weights=c(21,46,54,40,24,10,5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

