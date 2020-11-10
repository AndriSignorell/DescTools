\name{StrTrunc}
\alias{StrTrunc}
\title{
 Truncate Strings and Add Ellipses If a String is Truncated.
}
\usage{
StrTrunc(x, maxlen = 20, ellipsis = "...", wbound = FALSE)
}
\arguments{
 \item{x}{a vector of strings.}
 \item{maxlen}{the maximum length of the returned strings (NOT counting the appended ellipsis). \code{maxlen} is recycled.}
 \item{ellipsis}{the string to be appended, if the string is longer than the given maximal length. The default is \code{"..."}.}
 \item{wbound}{logical. Determines if the maximal length should be reduced to the next smaller word boundary and so words are not chopped. Default is \code{FALSE}. }
}
\description{
 Truncates one or more strings to a specified length, adding an ellipsis (...)
 to those strings that have been truncated. The truncation can also be performed using word boundaries.
 Use \code{\link{StrAlign}()} to justify the strings if needed.
}
\value{
 The string(s) passed as \samp{x} now with a maximum length of \samp{maxlen} + 3 (for the ellipsis).
}

\author{Andri Signorell, \cr once following an idea of Jim Lemon in \code{\link[prettyR]{truncString}()}}
\seealso{String functions: 
\code{\link{nchar}}, \code{\link{match}}, \code{\link{grep}}, \code{\link{regexpr}}, \code{\link{substr}}, \code{\link{sub}}, \code{\link{gsub}},
\code{\link{StrTrim}}, \code{\link{StrDist}}  
}

\examples{
x <- c("this is short", "and this is a longer text", 
       "whereas this is a much longer story, which could not be told shorter")

# simple truncation on 10 characters
StrTrunc(x, maxlen=10)

# NAs remain NA
StrTrunc(c(x, NA_character_), maxlen=15, wbound=TRUE)

# using word boundaries
for(i in -5:20)
  print(StrTrunc(x, maxlen=i, wbound=TRUE))

# compare
for(i in -5:20)
  print(StrTrunc(x, maxlen=i, wbound=FALSE))
}
\keyword{character}
\keyword{utilities}
