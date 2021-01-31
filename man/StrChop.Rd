\name{StrChop}
\alias{StrChop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a String into a Number of Sections of Defined Length
%%  ~~function to do ... ~~
}
\description{Splitting a string into a number of sections of defined length is needed, when we  want to split a table given as a number of lines without separator into columns. The cutting points can  either be defined by the lengths of the sections or directly by position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrChop(x, len, pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the string to be cut in pieces.
%%     ~~Describe \code{x} here~~
}
  \item{len}{a vector with the lengths of the pieces.}
  \item{pos}{a vector of cutting positions. Will be ignored when \code{len} has been defined.}

}
\details{If length is going over the end of the string the last part will be returned, so if the rest of the string is needed, it's possible to simply enter a big number as last partlength.

\code{len} and \code{pos} can't be defined simultaneously, only alternatively.

Typical usages are
\preformatted{
StrChop(x, len)
StrChop(x, pos)
}
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector with the parts of the string.
}
\author{Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{FixToTable}}, \code{\link{StrLeft}}, \code{\link{substr}}
}
\examples{
x <- paste(letters, collapse="")
StrChop(x=x, len = c(3,5,2))

# and with the rest integrated
StrChop(x=x, len = c(3, 5, 2, nchar(x)))

# cutpoints at 5th and 10th position
StrChop(x=x, pos=c(5, 10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character }
\concept{ string }
