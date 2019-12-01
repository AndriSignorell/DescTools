\name{Dummy}
\alias{Dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate Dummy Codes for a Factor
%%  ~~function to do ... ~~
}
\description{Generate a matrix of dummy codes (class indicators) for a given factor. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Dummy(x, method = c("treatment", "sum", "helmert", "poly", "full"),
      base = 1, levels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{factor or vector of classes for cases.
%%     ~~Describe \code{x} here~~
}
  \item{method}{defines the method of the contrasts being formed. Can be one
  out of \code{"treatment"}, \code{"sum"}, \code{"helmert"}, \code{"poly"}, \code{"full"},
  whereas \code{"treatment"} is the default one. Abbreviations are accepted.\cr
  The option \code{"full"} returns a full set of class indicators, say a dummy factor for \bold{each} level of x.
  Note that this would be redundant for \code{\link{lm}()} and friends!
%%     ~~Describe \code{method} here~~
}
  \item{base}{an integer specifying which group is considered the baseline group.
}
  \item{levels}{an optional vector of the values (as character strings) that \code{x} might have taken.
  The default is the unique set of values taken by as.character(x), sorted into increasing order of x.\cr
  This is directly passed on to \code{\link{factor}}.
}
}
\details{For reverting dummy codes see the approach in the examples below.}
\value{a matrix with the dummy codes.
The number of rows correspond to the number of elements in \code{x} and the number of columns to the number of its levels - 1, respectively to the number of levels given as argument -1.

When \code{method = "full"} is chosen the number of columns will correspond to the number of levels.
}
\references{
Venables, W N and Ripley, B D (2002): \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{model.frame}},  \code{\link{contrasts}}, \code{\link[nnet]{class.ind}} in the package \pkg{nnet}
}
\examples{
x <- c("red","blue","green","blue","green","red","red","blue")
Dummy(x)
Dummy(x, base=2)

Dummy(x, method="sum")


y <- c("Max","Max","Max","Max","Max","Bill","Bill","Bill")

Dummy(y)
Dummy(y, base="Max")

Dummy(y, base="Max", method="full")


# "Undummy" (revert the dummy coding)
m <- Dummy(y, method="full")
m
z <- apply(m, 1, function(x) colnames(m)[x==1])
z
identical(y, as.vector(z))

m <- Dummy(y)
m
z <- apply(m, 1, function(x) ifelse(sum(x)==0, attr(m,"base"), colnames(m)[x==1]))
z
}
\keyword{ math }
