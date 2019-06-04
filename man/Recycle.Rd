\name{Recycle}
\alias{Recycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recyle a List of Elements
%%  ~~function to do ... ~~
}
\description{This function recycles all supplied elments to the maximal dimension.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Recycle(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{a number of vectors of elements.
%%     ~~Describe \code{\dots} here~~
}
}
\value{a list of the supplied elements\cr
\code{attr(,"maxdim")} contains the maximal dimension of the recyled list
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{rep}}, \code{\link{replicate}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Recycle(x=1:5, y=1, s=letters[1:2])

z <- Recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]), collapse=z$sep[i]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
