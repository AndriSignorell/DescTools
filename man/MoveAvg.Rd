\name{MoveAvg}
\alias{MoveAvg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moving Average
%%  ~~function to do ... ~~
}
\description{Compute a simple moving average (running mean).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MoveAvg(x, order, align = c("center", "left", "right"), 
        endrule = c("NA", "keep", "constant"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{univariate time series.}
  \item{order}{order of moving average.}
  \item{align}{specifies whether result should be centered (default), left-aligned or right-aligned.}
  
 \item{endrule}{character string indicating how the values at the
    beginning and the end (of the data) should be treated.
    \describe{
      \item{\code{"keep"}}{keeps the first and last \eqn{k_2}{k2} values
	at both ends, where \eqn{k_2}{k2} is the half-bandwidth \code{k2
	  = k \%/\% 2},
	i.e., \code{y[j] = x[j]} for \eqn{j \in \{1,\ldots,k_2;
	  n-k_2+1,\ldots,n\}}{j = 1,..,k2 and (n-k2+1),..,n};}
      \item{\code{"constant"}}{fill the ends with first and last calculated value in output array
(out[1:k2] = out[k2+1])}
      \item{\code{"NA"}}{the default, leaves the values to NA, as they are returned by \code{\link{filter}}.}
    }}

}
\details{The implementation is using the function \code{filter} to calculate the moving average.}
\value{Returns a vector of the same size and same class as x. }

\author{Andri Signorell <andri@signorell.net> }

\seealso{There's a faster implementation of running mean in the package \pkg{caTools} \code{\link[caTools]{runmean}()} and a slower one in \pkg{forecast} \code{\link[forecast]{ma}()}. There's similar code in \code{\link{Midx}()}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MoveAvg(AirPassengers, order=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar }
