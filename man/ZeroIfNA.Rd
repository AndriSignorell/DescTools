\name{ZeroIfNA}
\alias{ZeroIfNA}
\alias{NAIfZero}
\alias{NAIfBlank}
\alias{NAVal}
\alias{NAIf}
\alias{Impute}

\title{Replacing NAs by Specific Values or Vice Versa}

\description{\code{ZeroIfNA()} replaces \code{NA}s in a numeric vector \code{x} with 0. 
This function has the same logic as the \code{ZEROIFNULL()} function in SQL. 
For values other than zero the function \code{NAVal()} can be used, which 
replaces the \code{NA} values by a given value. 
\code{NAIfZero()} does replace zeros with \code{NA}.
\code{NAIfBlank()} does the same, but for character vectors. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ZeroIfNA(x)
NAIfZero(x)
NAIf(x, what)

NAVal(x, val="")
NAIfBlank(x)

Impute(x, FUN = function(x) median(x, na.rm = TRUE))
}
\arguments{
  \item{x}{the vector x, whose \code{NA}s should be overwritten with 0s.}
  \item{val}{the value \code{NA}s should be replaced by. Default is an empty string ("").}
  \item{what}{a vector of elements to be set to \code{NA} in \code{x}.}
  \item{FUN}{the name of a function to be used as imputation. Can as well be a self defined function or a constant value.
  Default is \code{\link{median}}.}
}

\value{the edited vector x}

\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{replace}} }
\examples{
z <- c(8, NA, 9, NA, 3)

ZeroIfNA(z)
# [1] 8 0 9 0 3

# set 8 and 9 to NA
NAIf(ZeroIfNA(z), what=c(8, 9))


Impute(z)
# [1] 8 8 9 8 3


z <- c("a", NA, "", NA, "k")

NAVal(z)
# [1] "a" "" "" "" "k"

}
\keyword{ manip }
