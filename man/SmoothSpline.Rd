\name{SmoothSpline}
\alias{SmoothSpline}
\alias{SmoothSpline.formula}
\alias{SmoothSpline.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula Interface For \code{smooth.spline}
%%  ~~function to do ... ~~
}
\description{smooth.spline has no formula interface, which is sometimes inconvenient, if one simply wants to copy a formula of a linear model or a plot to spline.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

SmoothSpline(x, ...)

\method{SmoothSpline}{default}(x, y = NULL, w = NULL, df, spar = NULL, cv = FALSE,
             all.knots = FALSE, nknots = .nknots.smspl, keep.data = TRUE,
             df.offset = 0, penalty = 1, control.spar = list(),
             tol = 0.000001 * IQR(x), ...)

\method{SmoothSpline}{formula}(formula, data, subset, na.action, \dots)


}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{x}{a vector giving the values of the predictor variable, or a
 list or a two-column matrix specifying x and y. }
 \item{y}{responses. If \code{y} is missing or \code{NULL}, the responses
 are assumed to be specified by \code{x}, with \code{x} the index
 vector.}
 \item{w}{optional vector of weights of the same length as \code{x};
 defaults to all 1.}
 \item{df}{the desired equivalent number of degrees of freedom (trace of
 the smoother matrix).}
 \item{spar}{smoothing parameter, typically (but not necessarily) in
 \eqn{(0,1]}. The coefficient \eqn{\lambda} of the integral of the
 squared second derivative in the fit (penalized log likelihood)
 criterion is a monotone function of \code{spar}, see the details
 below.}
 \item{cv}{ordinary (\code{TRUE}) or \sQuote{generalized} cross-validation
 (GCV) when \code{FALSE}; setting it to \code{NA} skips the evaluation
 of leverages and any score.}
 \item{all.knots}{if \code{TRUE}, all distinct points in \code{x} are used as
 knots. If \code{FALSE} (default), a subset of \code{x[]} is used,
 specifically \code{x[j]} where the \code{nknots} indices are evenly
 spaced in \code{1:n}, see also the next argument \code{nknots}.}
 \item{nknots}{integer or \code{\link{function}} giving the number of
 knots to use when \code{all.knots = FALSE}. If a function (as by
 default), the number of knots is \code{nknots(nx)}. By default for
 \eqn{n_x > 49}{nx > 49} this is less than \eqn{n_x}{nx}, the number
 of unique \code{x} values, see the Note.}
 \item{keep.data}{logical specifying if the input data should be kept
 in the result. If \code{TRUE} (as per default), fitted values and
 residuals are available from the result.}
 \item{df.offset}{allows the degrees of freedom to be increased by
 \code{df.offset} in the GCV criterion.}
 \item{penalty}{the coefficient of the penalty for degrees of freedom
 in the GCV criterion.}

 \item{control.spar}{optional list with named components controlling the
 root finding when the smoothing parameter \code{spar} is computed,
 i.e., missing or \code{NULL}, see below.

 \bold{Note} that this is partly \emph{experimental} and may change
 with general spar computation improvements!

 \describe{
 \item{low:}{lower bound for \code{spar}; defaults to -1.5 (used to
 implicitly default to 0 in \R versions earlier than 1.4).}
 \item{high:}{upper bound for \code{spar}; defaults to +1.5.}
 \item{tol:}{the absolute precision (\bold{tol}erance) used; defaults
 to 1e-4 (formerly 1e-3).}
 \item{eps:}{the relative precision used; defaults to 2e-8 (formerly
 0.00244).}
 \item{trace:}{logical indicating if iterations should be traced.}
 \item{maxit:}{integer giving the maximal number of iterations;
 defaults to 500.}
 }
 Note that \code{spar} is only searched for in the interval
 \eqn{[low, high]}.
 }

 \item{tol}{a tolerance for same-ness or uniqueness of the \code{x}
 values. The values are binned into bins of size \code{tol} and
 values which fall into the same bin are regarded as the same. Must
 be strictly positive (and finite).}

  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the                 corresponding groups.}

  \item{data}{The data frame from which the formula should
            be evaluated.}

  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to
         \code{getOption("na.action")}.}

  \item{\dots}{Other arguments to be passed to
        \code{\link{smooth.spline}}.}

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{smooth.spline}}, \code{\link{lines.smooth.spline}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
plot(temperature ~ delivery_min, data=d.pizza)
lines(SmoothSpline(temperature ~ delivery_min, data=d.pizza))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ smooth }
