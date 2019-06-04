\name{ZeroIfNA}
\alias{ZeroIfNA}
\alias{NAIfZero}
\alias{BlankIfNA}
\alias{NAIfBlank}
\alias{Impute}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Replace NAs by 0
%%  ~~function to do ... ~~
}
\description{Replace NAs in a numeric vector x with 0. This function has the same logic as the zeroifnull function in SQL. \code{NAIfZero()} does replace zeros with \code{NA}.
\code{BlankIfNA()} and \code{NAIfBlank()} do the same, but for character vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ZeroIfNA(x)
NAIfZero(x)

BlankIfNA(x)
NAIfBlank(x)

Impute(x, FUN = function(x) median(x, na.rm = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector x, whose NAs should be overwritten with 0s.
%%     ~~Describe \code{x} here~~
}
  \item{FUN}{the name of a function to be used as imputation. Can as well be a self defined function or a constant value.
  Default is \code{\link{median}}.

%%     ~~Describe \code{na.rm} here~~
}
}
\value{the edited vector x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{replace}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z <- c(8, NA, 9, NA, 3)

ZeroIfNA(z)
# [1] 8 0 9 0 3

Impute(z)
# [1] 8 8 9 8 3


z <- c("a", NA, "", NA, "k")

BlankIfNA(z)
# [1] "a" "" "" "" "k"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }% __ONLY ONE__ keyword per line
