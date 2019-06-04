\name{VecRot}
\alias{VecRot}
\alias{VecShift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Rotation (Shift Elements)
%%  ~~function to do ... ~~
}
\description{Shift the elements of a vector in circular mode by \code{k} elements to the right (for positive k) or to the left (for negative k), such that the first element is at the (k+1)th position of the new vector and the last k elements are appended to the beginning.\cr \code{VecShift} does not attach the superfluous elements on one side to the other, but fills the resulting gaps with \code{NA}s.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VecRot(x, k = 1)
VecShift(x, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of any type.
%%     ~~Describe \code{x} here~~
}
  \item{k}{the number of elements to shift.
%%     ~~Describe \code{n} here~~
}
}
\details{The function will repeat the vector two times and select the appropriate number of elements from the required shift on.

%%  ~~ If necessary, more details than the description above ~~
}
\value{the shifted vector in the same dimensions as x.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{[}}, \code{\link{rep}}, \code{\link{lag}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
VecRot(c(1,1,0,0,3,4,8), 3)

VecRot(letters[1:10], 3)
VecRot(letters[1:10], -3)

VecShift(letters[1:10], 3)
VecShift(letters[1:10], -3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
