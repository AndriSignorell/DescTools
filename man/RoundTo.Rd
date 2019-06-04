\name{RoundTo}
\alias{RoundTo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Round to Multiple
%%  ~~function to do ... ~~
}
\description{Returns a number rounded to the nearest specified multiple.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RoundTo(x, multiple = 1, FUN = round)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric. The value to round.
%%     ~~Describe \code{x} here~~
}
  \item{multiple}{numeric. The multiple to which the number is to be rounded. Default is 1.
%%     ~~Describe \code{multiple} here~~
}
\item{FUN}{the rounding function as character or as expression. Can be one out of \code{\link{trunc}}, \code{ceiling}, \code{round} (default) or \code{floor}. }
}
\details{There are several functions to convert to integers. \code{\link{round}} rounds to the nearest integer or to any number of digits. Using a negative number rounds to a power of ten, so that \code{round (x, -3)} rounds to thousands.
Each of \code{\link{trunc}}, \code{\link{floor}} and \code{\link{ceiling}} round in a fixed direction, towards zero, down and up respectively. \code{\link{round}} is documented to round to even, so \code{round(2.5)} is \code{2}.

\code{\link{RoundTo}} uses \code{round(x/multiple)*multiple} to get the result. So if \code{x} is equally close to two multiples, the multiple with the smaller absolute value will be returned when \code{round(x/multiple)} is even (and the greater when it's odd).\cr
If \code{FUN} is set to \code{ceiling} it will always round up, and if set to \code{floor} it will always round down. See examples for comparison).
%%  ~~ If necessary, more details than the description above ~~
}
\value{the rounded value
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

\seealso{\code{\link{round}}, \code{\link{trunc}}, \code{\link{ceiling}}, \code{\link{floor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
RoundTo(10, 3)     # Rounds 10 to a nearest multiple of 3 (9)
RoundTo(-10, -3)   # Rounds -10 to a nearest multiple of -3 (-9)

RoundTo(1.3, 0.2)  # Rounds 1.3 to a nearest multiple of 0.2 (1.2)
RoundTo(-1.3, 0.2) # Rounds -1.3 to a nearest multiple of 0.2 (-1.2)
RoundTo(5, -2)     # Returns an error, because -2 and 5 have different signs

# Round down
RoundTo(c(1,-1) * 1.2335, 0.05, floor)
RoundTo(c(1,-1) * 1233.5, 100, floor)

# Round up
RoundTo(c(1,-1) * 1.2335, 0.05, ceiling)
RoundTo(c(1,-1) * 1233.5, 100, ceiling)

# Round towards zero
RoundTo(c(1,-1) * 1.2335, 0.05, trunc)
RoundTo(c(1,-1) * 1233.5, 100, trunc)


x <- c(-1.5,-1.3, 1.3, 1.5)
cbind(x =       x,
      round =   RoundTo(x, 0.2, FUN=round),
      trunc =   RoundTo(x, 0.2, FUN=trunc),
      ceiling = RoundTo(x, 0.2, FUN=ceiling),
      floor =   RoundTo(x, 0.2, FUN=floor)
)

x <- -10:10
cbind(x =       x,
      round =   RoundTo(x, 2, FUN=round),
      trunc =   RoundTo(x, 2, FUN=trunc),
      ceiling = RoundTo(x, 2, FUN=ceiling),
      floor =   RoundTo(x, 2, FUN=floor)
)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

