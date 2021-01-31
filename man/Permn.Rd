\name{Permn}
\alias{Permn}
\alias{CombSet}
\alias{CombN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number and Samples for Permutations or Combinations of a Set
%%  ~~function to do ... ~~
}
\description{ Return the set of permutations for a given set of values.
The values can be numeric values, characters or factors. \code{CombN} computes the number of combinations with and without replacement and order, whereas \code{CombSet} returns the value sets.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Permn(x, sort = FALSE)

CombN(n, m, repl = FALSE, ord = FALSE)
CombSet(x, m, repl = FALSE, ord = FALSE, as.list = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of numeric values or characters. Characters need not be unique.
%%     ~~Describe \code{x} here~~
}
  \item{n}{number of elements from which to choose.}
  \item{m}{number of elements to choose. For \code{CombSet} can \code{m} be a numeric vector too.}
  \item{repl}{logical. Should repetition of the same element be allowed? Defaults to FALSE}
  \item{ord}{logical. Does the order matter? Default is FALSE.}

  \item{sort}{logical, defining if the result set should be sorted. Default is FALSE.
%%     ~~Describe \code{x} here~~
}
\item{as.list}{logical, defining if the results should be returned in a flat list, say every sample is a single element of the resulting list. Default is FALSE.}
}
\details{The vector x need not contain unique values. The permutations will
automatically be filtered for unique sets, if the same element is given twice or more.}
\value{a matrix with all possible permutations or combinations of the values in x for \code{Permn} and \code{CombSet}\cr
if m contains more than one element the result will be a list of matrices or a flat list if \code{as.list} is set to \code{TRUE}
\cr
an integer value for \code{CombN}
}
\author{Friederich Leisch <Friedrich.Leisch@boku.ac.at>\cr Andri Signorell <andri@signorell.net> (CombSet, CombN)
%%  ~~who you are~~
}

\seealso{\code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}}, \code{\link{CombPairs}}\cr
\code{vignette("Combinatorics")}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Permn(letters[2:5])
Permn(2:5)

# containing the same element more than once
Permn(c("a", "b", "c", "a"))


# only combinations of 2, but in every possible order
x <- letters[1:4]
n <- length(x)
m <- 2

# the samples
CombSet(x, m, repl=TRUE, ord=FALSE)
CombSet(x, m, repl=TRUE, ord=TRUE)
CombSet(x, m, repl=FALSE, ord=TRUE)
CombSet(x, m, repl=FALSE, ord=FALSE)

# the number of the samples
CombN(n, m, repl=TRUE, ord=FALSE)
CombN(n, m, repl=TRUE, ord=TRUE)
CombN(n, m, repl=FALSE, ord=TRUE)
CombN(n, m, repl=FALSE, ord=FALSE)

# build all subsets of length 1, 3 and 5 and return a flat list
x <- letters[1:5]
CombSet(x=x, m=c(1, 3, 5), as.list=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\concept{ combinatorics }

