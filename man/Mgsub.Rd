\name{Mgsub}
\alias{Mgsub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple Gsub
%%  ~~function to do ... ~~
}
\description{Performs multiple substitions in (a) string(s).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mgsub(pattern, replacement, x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. 
}
  \item{replacement}{a replacement for matched pattern as in \code{\link{sub}} and \code{\link{gsub}}.
  See there for more information.
}
  \item{x}{a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
}
  \item{\dots}{all dots are passed on to gsub.
}
}
\value{
a character vector of the same length and with the same attributes as x (after possible coercion to character). 
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{gsub}}
}
\examples{
x <- c("ABC", "BCD", "CDE")
Mgsub(pattern=c("B", "C"), replacement=c("X","Y"), x)
}
