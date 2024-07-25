\name{Hmean}
\alias{Hmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Harmonic Mean and Its Confidence Interval
%%  ~~function to do ... ~~
}
\description{Calculates the harmonic mean and its confidence interval of a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Hmean(x, method = c("classic", "boot"), conf.level = NA,
      sides = c("two.sided","left","right"), na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a positive numeric vector. An object which is not a vector is coerced (if possible) by as.vector.
%%     ~~Describe \code{x} here~~
}
  \item{method}{a vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"boot"}.
  See \code{\link[boot]{boot.ci}}.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval. Default is \code{NA}.
%%     ~~Describe \code{conf.level} here~~
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}


  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
 \item{...}{further arguments are passed to the \code{\link[boot]{boot}} function. Supported arguments are \code{type} (\code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number of bootstrap replicates \code{R}. If not defined those will be set to their defaults, being \code{"basic"} for \code{type},  option \code{"boot.parallel"} (and if that is not set, \code{"no"}) for \code{parallel}
 and \code{999} for \code{R}.%%     ~~Describe \code{x} here~~
}

}
\details{

To compute the harmonic mean, \code{1/x} is first calculated, before the arithmetic mean and its confidence interval are computed by \code{\link{MeanCI}}. The harmonic mean is then the reciprocal of the arithmetic mean of the reciprocals of the values. The same applies to the confidence interval.

The harmonic mean is restricted to strictly positive inputs, if any argument is negative, then the result will be \code{NA}.
If the lower bound of the confidence interval is not greater than zero, then the confidence interval is not defined, and thus  \code{NA} will be reported.

Use \code{\link{sapply}} to calculate the measures from data frame, resp. from a matrix. \cr
}
\value{
a numeric value.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Gmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\references{Snedecor, G. W., Cochran, W. G. (1989) Statistical Methods, 8th ed. Ames, \emph{IA: Iowa State University Press }}


\examples{
x <- runif(5)
Hmean(x)

m <- matrix(runif(50), nrow = 10)
apply(m, 2, Hmean)

sapply(as.data.frame(m), Hmean)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

