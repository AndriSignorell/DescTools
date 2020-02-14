\name{AllIdentical}
\alias{AllIdentical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test Multiple Objects for Exact Equality
%%  ~~function to do ... ~~
}
\description{The function \code{\link{identical}()} is the safe and reliable way to test two objects for being exactly equal. But it is restricted to the comparison of two objects. \code{AllIdentical()} allows the input of multiple objects and returns \code{TRUE} in the case that all of them are exactly equal, \code{FALSE} in every other case.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllIdentical(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{any \code{R} objects}
}
\details{
The function checks the first object against all others, so if the first object is identical to the second and to the third, then also the second and the third are identical.
(If A=B and A=C then is B=C)
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{identical}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
A <- LETTERS[1:5]
B <- LETTERS[1:5]
C <- LETTERS[1:5]
D <- LETTERS[1:5]
E <- factor(LETTERS[1:5])

# all ok
AllIdentical(A, B, C, D)

# at least one odd man
AllIdentical(A, B, C, D, E)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ logic }
\keyword{ programming }
\keyword{ IO }
\keyword{ iteration }

