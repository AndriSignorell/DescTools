\name{Gmean}
\alias{Gmean}
\alias{Gsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Mean and Standard Deviation
%%  ~~function to do ... ~~
}
\description{Calculates the geometric mean, its confidence interval and the geometric standard deviation of a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gmean(x, method = c("classic", "boot"), conf.level = NA,
      sides = c("two.sided","left","right"), na.rm = FALSE, ...)

Gsd(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a positive numeric vector. An object which is not a vector is coerced (if possible) by as.vector.
%%     ~~Describe \code{x} here~~
}
  \item{method}{a vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"boot"}.
  See \code{\link{boot.ci}}.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval. Default is \code{NA}.
%%     ~~Describe \code{conf.level} here~~
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}


  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
 \item{...}{further arguments are passed to the \code{\link{boot}} function. Supported arguments are \code{type} (\code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number of bootstrap replicates \code{R}. If not defined those will be set to their defaults, being \code{"basic"} for \code{type},  option \code{"boot.parallel"} (and if that is not set, \code{"no"}) for \code{parallel}
 and \code{999} for \code{R}.%%     ~~Describe \code{x} here~~
}
}
\details{
To compute the geometric mean, \code{log(x)} is first calculated, before the arithmetic mean and its confidence interval are computed by \code{\link{MeanCI}}.

So the geometric mean and geometric sd are restricted to positive inputs (because otherwise the answer can have an imaginary component). Hence if any argument is negative, the result will be \code{NA}. If any argument is zero, then the geometric mean is zero.\cr

The geometric mean is defined as
\deqn{\sqrt[n]{x_{1}\cdot x_{2}\cdot x_{3} \ldots \cdot x_{n}}}{(x_1 * x_2 * ... * x_n)^(1/n)}

and its confidence interval is given as \code{exp(MeanCI(log(x)))}.
Use \code{\link{sapply}} to calculate the measures from data frame, resp. from a matrix. \cr
}
\value{a numeric value.
}
\references{Snedecor, G. W., Cochran, W. G. Cochran (1989) Statistical Methods, 8th ed. Ames, \emph{IA: Iowa State University Press }}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{mean}}, \code{\link{Hmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- runif(5)
Gmean(x)

m <- matrix(runif(50), nrow = 10)
apply(m, 2, Gmean)

sapply(as.data.frame(m), Gmean)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

