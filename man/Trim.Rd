\name{Trim}
\alias{Trim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trim a Vector
%%  ~~function to do ... ~~
}
\description{Clean data by means of trimming, i.e., by omitting
  outlying observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Trim(x, trim = 0.1, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector to be trimmed.
%%     ~~Describe \code{x} here~~
}
  \item{trim}{the fraction (0 to 0.5) of observations to be trimmed from each end of x. Values of trim outside that range (and < 1) are taken as the nearest endpoint.
   If \code{trim} is set to a value >1 it's interpreted as the number of elements to be cut off at each tail of \code{x}.
%%     ~~Describe \code{trim} here~~
}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
%%     ~~Describe \code{na.rm} here~~
}
}

\details{A symmetrically trimmed vector \code{x} with a fraction of trim observations (resp. the given number) deleted from each end will be returned. If \code{trim} is set to a value >0.5 or to an integer value > n/2 then the result will be \code{NA}. }

\value{
The trimmed vector \code{x}. The indices of the trimmed values will be attached as attribute named \code{"trim"}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{This function is basically an excerpt from the base function \code{\link{mean}}, which allows the vector \code{x} to be trimmed before calculating the mean. But what if a trimmed standard deviation is needed?}
\author{R-Core (function mean), Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Winsorize}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## generate data
set.seed(1234)     # for reproducibility
x <- rnorm(10)     # standard normal
x[1] <- x[1] * 10  # introduce outlier

## Trim data
x
Trim(x, trim=0.1)

## Trim fixed number, say cut the 3 extreme elements from each end
Trim(x, trim=3)

## check function
s <- sample(10:20)
s.tr <- Trim(s, trim = 2)
setequal(c(s[attr(s.tr, "trim")], s.tr), s)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
\keyword{ robust }
