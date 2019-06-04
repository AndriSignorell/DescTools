\name{AscToChar}
\alias{AscToChar}
\alias{CharToAsc}

\title{Convert ASCII Codes to Characters and Vice Versa
}
\description{AscToChar returns a character for each ASCII code (integer) supplied.\cr
CharToAsc returns integer codes in \code{0:255} for each (one byte) character in all strings in \code{x}.
}
\usage{
AscToChar(i)
CharToAsc(x)
}
\arguments{
\item{i}{numeric (integer) vector of values in \code{1:255}.
}
\item{x}{vector of strings.
}}
\details{Only codes in \code{1:127} make up the ASCII encoding which should be
  identical for all \R versions, whereas the \emph{\sQuote{upper}} half
  is often determined from the ISO-8859-1 (aka \dQuote{ISO-Latin 1)}
  encoding, but may well differ, depending on the locale setting, see
  also \code{\link{Sys.setlocale}}.

  Note that \code{0} is no longer allowed since, \R does not allow
  \code{\\0} aka \code{nul} characters in a string anymore.
}
\value{
 \code{AscToChar} returns a vector of the same length as i.
 \code{CharToAsc} returns a list of numeric vectors of character length of each string in x.
}
\author{unknown guy out there, help text partly taken from M. Maechler's \pkg{sfsmisc}.
}
\seealso{\code{\link{charToRaw}}}

\examples{
(x <- CharToAsc("Silvia"))

# will be pasted together
AscToChar(x)

# use strsplit if the single characters are needed
strsplit(AscToChar(x), split=NULL)

# this would be an alternative, but the latter would be of class raw
DecToHex(CharToAsc("Silvia"))
charToRaw("Silvia")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
