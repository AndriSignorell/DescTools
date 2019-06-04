\name{IsDichotomous}
\alias{IsDichotomous}
\alias{Flags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test If a Variable Contains Only Two Unique Values
%%  ~~function to do ... ~~
}
\description{Test if a variable contains only two values. The variable does not need to be a numerical value, factors and logicals are supported as well. \code{NA}s can be skipped by setting \code{na.rm} to \code{TRUE}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsDichotomous(x, strict = FALSE, na.rm = FALSE)

Flags(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric or integer vector, a logical vector or a factor (ordered and unordered)}
  \item{strict}{logical. If set to \code{TRUE}, the result will only be \code{TRUE}, if x contains exactly 2 levels. If set to \code{FALSE} the result will be \code{TRUE} for 1 and for 2 levels.}

\item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.}
}

\details{IsDichotomous tests a single variable. Flags returns the names of all the dichotomous variables in a list or data.frame.}
\value{\code{TRUE} if \code{x} contains only two unique values, \code{FALSE} else
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
IsDichotomous(sample(10, 5, replace=TRUE))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
