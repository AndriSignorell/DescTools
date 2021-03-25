\name{StrCap}
\alias{StrCap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Capitalize the First Letter of a String}
\description{
Capitalize the first letter of each element of the string vector.
}
\usage{
StrCap(x, method=c("first", "word", "title"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ string to be capitalized. }
  \item{method}{one out of \code{"first"} (default), \code{"word"}, \code{"title"}. \code{"first"} will only capitalize the first character of a string. \code{"word"} will capitalize all found words and \code{"title"} will also capitalize wordwise, but leave out: a, an, the, at, by, for, in, of, on, to, up, and, as, but, s, or and nor.) }
}
\value{
  Returns a vector of charaters with the first letter capitalized
}
\author{ Charles Dupont  <charles.dupont@vanderbilt.edu>, Andri Signorell <andri@signorell.net> (methods word and title)}
\examples{
# capitalize first character
StrCap(c("Hello", "bob", "daN"))
# but not all...
StrCap(c("Hello bob, how are you?", "And you, DANIEL?"))

# wordwise
StrCap(c("Capitalize all words in titles of publications and documents",
              "but Up and UP, not all and all", NA), method="word")

# wordwise omitting the ones listed above
StrCap(c("Capitalize all words in titles of publications and documents",
         "but Up and UP, not all and all", NA), method="title")

# do not touch non alphabetic characters
z <- c("Lorem ipsum dolor", "-- sit amet", "consectetur --", " adipiscing elit ",
       "sed,.--(do) / +-*eiusmod")
StrCap(z, method="title")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
\concept{ string }
