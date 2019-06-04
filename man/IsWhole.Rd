\name{Some numeric checks}
\alias{IsWhole}
\alias{IsNumeric}
\alias{IsZero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check a Vector For Being Numeric, Zero Or a Whole Number
%%  ~~function to do ... ~~
}
\description{Test if x contains only integer numbers, or if is numeric or if it is zero.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsWhole(x, all = FALSE, tol = sqrt(.Machine$double.eps), na.rm = FALSE)
IsZero(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE)
IsNumeric(x, length.arg = Inf, integer.valued = FALSE, positive = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
\item{all}{logical, specifying if the whole vector should be checked. If set to \code{TRUE} the function will
return the result of \code{all(IsWhole(x))}. }
  \item{tol}{tolerance to be used
%%     ~~Describe \code{tol} here~~
}
  \item{length.arg}{integer, the length of the vector to be checked for.}
  \item{integer.valued}{logical, should x be checked as integer?}
  \item{positive}{logical, is x supposed to be positive?}

  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}

}
\details{
IsWhole is the suggested solution for checking for an integer value, as \code{\link{is.integer}} tests for \code{class(x) == "integer"} and does NOT test whether x (which might be of class "numeric") contains only integer numbers.
(Why not simply implement it in \pkg{base}?)

IsZero tests float numeric values for being zero.

IsNumeric combines a test for numeric and integers.
%%  ~~ If necessary, more details than the description above ~~
}
\value{logical vector of the same dimension as x.
}
\author{R-Core, Andri Signorell <andri@signorell.net>, Thomas W. Yee
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.integer}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
(x <- seq(1,5, by=0.5))
IsWhole( x ) #-->  \code{TRUE} \code{FALSE} \code{TRUE} ...


# ... These are people who live in ignorance of the Floating Point Gods.
# These pagans expect ... (Burns, 2011)" the following to be TRUE:
(.1 - .3 / 3) == 0

# they might be helped by
IsZero(.1 - .3 / 3)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ logic }




