\name{TheilU}
\alias{TheilU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Theil's U Index of Inequality
%%  ~~function to do ... ~~
}
\description{Calculate Theil's U index of inequality.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TheilU(a, p, type = c(2, 1), na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{a numeric vector with the actual observed values.
%%     ~~Describe \code{x} here~~
}
  \item{p}{a numeric vector containing the predictions.
%%     ~~Describe \code{y} here~~
}
  \item{type}{defining the type of Theil's two U measures, see Details. Default is 2.
%%     ~~Describe \code{y} here~~
}
\item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. If set to \code{TRUE} complete cases of \code{cbind(x, y)} will be used. Defaults to \code{FALSE}.
  %%     ~~Describe \code{na.rm} here~~
}
}

\details{Theil proposed two error measures, but at different times and under the same symbol U, which has caused some confusion.
U \code{type = 1} is taken from Theil  (1958, pp. 31-42). The argument \code{a} represents the actual observations and \code{p} the corresponding predictions. He left it open whether \code{a} and \code{p} should be used as absolute values or as observed and predicted changes. \cr
Theil (1966, chapter 2) proposed U \code{type = 2} as a measure of forecast quality: \emph{"...where \eqn{A_i} and \eqn{P_i} stand for a pair of predicted and observed changes. ..."} \cr
As \eqn{U_1} has some serious disadvantages (see Bliemel 1973) it is recommended to use \eqn{U_2}.
%%  ~~ If necessary, more details than the description above ~~
}

\references{
Theil, H. (1958): \emph{Economic Forecasts and Policy}. Amsterdam: North Holland.

Thiel, H. (1966): \emph{Applied Economic Forecasting}. Chicago: Rand McNally.

Bliemel, F. (1973): Theil's Forecast Accuracy Coefficient: A Clarification, \emph{Journal of Marketing Research} Vol. 10, No. 4 (Nov., 1973), pp. 444-446

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Gini}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
TheilU(1:10, 2:11, type=1)
TheilU(1:10, 2:11, type=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar }
