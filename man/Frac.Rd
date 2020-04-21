\name{Frac}
\alias{Frac}
\alias{MaxDigits}
\alias{Ndec}
\alias{Prec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fractional Part and Maximal Digits of a Numeric Value
%%  ~~function to do ... ~~
}
\description{\code{Frac()} returns the fractional part of a numeric value.
\code{MaxDigits()} return the number of digits in \code{x}. \cr
\code{Ndec()} returns the number of decimals.\cr  \code{Prec()} returns the precision of a number \code{x}.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Frac(x, dpwr = NA)
MaxDigits(x)
Ndec(x)
Prec(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric value (or a vector of numerics), whose fractional part is to be calculated.
%%     ~~Describe \code{x} here~~
}
  \item{dpwr}{power of 10 for a factor z, the fractional part will be multiplied with. The result will be returned rounded to integer. Defaults to \code{NA} and will then be ignored.
%%     ~~Describe \code{d} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{format.info}}, \code{\link{as.integer}}, \code{\link{trunc}}
}
\examples{
x <- rnorm(5)*100
x
Frac(x)

# multiply by 10^4
Frac(x, dpwr=4)

MaxDigits(c(1.25, 1.8, 12.0, 1.00000))

x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
Ndec(x)
Prec(as.numeric(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

