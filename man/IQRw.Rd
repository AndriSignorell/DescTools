\name{IQRw}
\alias{IQRw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The (weighted) Interquartile Range
%%  ~~function to do ... ~~
}
\description{computes interquartile range of the x values. Weights are supported.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IQRw(x, weights = NULL, na.rm = FALSE, type = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.
%%     ~~Describe \code{x} here~~
}
  \item{weights}{an optional numeric vector giving the sample weights.
%%     ~~Describe \code{weights} here~~
}
  \item{na.rm}{logical. Should missing values be removed?
%%     ~~Describe \code{na.rm} here~~
}
  \item{type}{an integer selecting one of the many quantile algorithms, see \code{\link{Quantile}()}.
}
}
\details{This implementation is based on \code{\link{Quantile}()} function, which allows to define weights.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{Median}()}, \code{\link{Quantile}()}, \code{\link{IQR}()}, \code{\link{quantile}()} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(3.7,3.3,3.5,2.8)
w <- c(5, 5, 4, 1)/15

IQRw(x=x, weights=w)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
\keyword{ robust }
\keyword{ distribution }
\keyword{ univar }
