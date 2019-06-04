\name{StrExtract}
\alias{StrExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Part of a String
%%  ~~function to do ... ~~
}
\description{Extract a part of a string, defined as regular expression.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrExtract(x, pattern, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector where matches are sought, or an object which can be coerced by \code{as.character} to a character vector.
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{character string containing a regular expression (or character string for \code{fixed = TRUE}) to be matched in the given character vector. Coerced by \code{\link{as.character}} to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are not allowed.
%%     ~~Describe \code{pattern} here~~
}
\item{\dots}{the dots are passed to the the internally used function \code{\link{regexpr}()}, which allows to use e.g. Perl-like regular expressions.}
}
\details{The function wraps \code{\link{regexpr}} and \code{\link{regmatches}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A character vector.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{regexpr}}, \code{\link{regmatches}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
txt <- c("G1:E001", "G2:E002", "G3:E003")
# extract everything after the :
StrExtract(":.*", txt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{character}
\keyword{utilities}
