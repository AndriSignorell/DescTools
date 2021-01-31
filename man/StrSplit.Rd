\name{StrSplit}
\alias{StrSplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split the Elements of a Character Vector
%%  ~~function to do ... ~~
}
\description{Split the elements of a character vector x into substrings according to the matches to substring split within them.\cr
This is a verbatim copy of the base R function \code{\link{strsplit}}, but with a split default of \code{""} and returning a vector instead of a list, when x had the length 1.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrSplit(x, split = "", fixed = FALSE, perl = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    character vector, each element of which is to be split.  Other
    inputs, including a factor, will give an error.
  }
  \item{split}{
    character vector (or object which can be coerced to such)
    containing \link{regular expression}(s) (unless \code{fixed = TRUE})
    to use for splitting.  If empty matches occur, in particular if
    \code{split} has length 0, \code{x} is split into single characters.
    If \code{split} has length greater than 1, it is re-cycled along
    \code{x}.
  }
  \item{fixed}{
    logical.  If \code{TRUE} match \code{split} exactly, otherwise
    use regular expressions.  Has priority over \code{perl}.
  }
  \item{perl}{logical.  Should Perl-compatible regexps be used?}
  \item{useBytes}{logical.  If \code{TRUE} the matching is done
    byte-by-byte rather than character-by-character, and inputs with
    marked encodings are not converted.  This is forced (with a warning)
    if any input is found which is marked as \code{"bytes"}
    (see \code{\link{Encoding}}).}
}
\details{
See \code{\link{strsplit}} for the details.
}
\value{
  A list of the same length as \code{x}, the \code{i}-th element of which
  contains the vector of splits of \code{x[i]}.
  
  If the length x was 1 a vecotor with the splits will be returned.
  
}

\seealso{
  \code{\link{paste}} for the reverse,
  \code{\link{grep}} and \code{\link{sub}} for string search and
  manipulation; also \code{\link{nchar}}, \code{\link{substr}}.

  \sQuote{\link{regular expression}} for the details of the pattern
  specification.
}
\examples{
noquote(StrSplit("A text I want to display with spaces"))

# the same as ...
noquote(strsplit("A text I want to display with spaces", NULL)[[1]])
}
\keyword{character}
\concept{ string }
