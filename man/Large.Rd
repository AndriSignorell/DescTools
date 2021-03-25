\name{Extremes}
\alias{Large}
\alias{Small}

\alias{HighLow}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kth Smallest/Largest Values
%%  ~~function to do ... ~~
}
\description{Find the kth smallest, resp. largest values from a vector \code{x} and return the values and their frequencies.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Small(x, k = 5, unique = FALSE, na.last = NA)
Large(x, k = 5, unique = FALSE, na.last = NA)

HighLow(x, nlow = 5, nhigh = nlow, na.last = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{numeric} vector
%%     ~~Describe \code{x} here~~
}
  \item{k}{an integer >0 defining how many extreme values should be returned. Default is \code{k = 5}. If \code{k > length(x)}, all values will be returned.
%%     ~~Describe \code{k} here~~
}
  \item{unique}{logical, defining if unique values should be considered or not. If this is set to \code{TRUE}, a list with the \code{k} extreme values and their frequencies is returned. Default is \code{FALSE} (as unique is a rather expensive function).
%%     ~~Describe \code{unique} here~~
}
\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}

  \item{nlow}{a single integer. The number of the smallest elements of a vector to be printed. Defaults to 5.
}
  \item{nhigh}{a single integer. The number of the greatest elements of a vector to be printed. Defaults to the number of \code{nlow}.
}

}
\details{This does not seem to be a difficult problem at first sight. We could simply tabulate and sort the vector and finally take the first or last k values. However sorting and tabulating the whole vector when we're just interested in the few smallest values is a considerable waste of resources. This approach becomes already impracticable for medium vector lengths (~\ifelse{html}{\out{10<sup>5</sup>}}{\eqn{10^5}}). There are several points and solutions of this problem discussed out there.
The present implementation is based on highly efficient C++ code and proved to be very fast.

HighLow combines the two upper functions and reports the k extreme values on both sides together with their frequencies in parentheses. It is used for describing univariate variables and is interesting for checking the ends of the vector, where in real data often wrong values accumulate.
This is in essence a printing routine for the highest and the lowest values of x.
}
\value{if \code{unique} is set to \code{FALSE}: a vector with the k most extreme values,
\cr
else: a list, containing the k most extreme values and their frequencies.
}
\author{Andri Signorell <andri@signorell.net>\cr
C++ parts by Nathan Russell and Romain Francois
}

\references{\url{https://stackoverflow.com/questions/36993935/find-the-largest-n-unique-values-and-their-frequencies-in-r-and-rcpp/}

\url{https://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{max}}, \code{\link{max}}, \code{\link{sort}}, \code{\link{rank}}
}
\examples{
x <- sample(1:10, 1000, rep=TRUE)
Large(x, 3)
Large(x, k=3, unique=TRUE)

# works fine up to x ~ 1e6
x <- runif(1000000)
Small(x, 3, unique=TRUE)
Small(x, 3, unique=FALSE)

# Both ends
cat(HighLow(d.pizza$temperature, na.last=NA))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
