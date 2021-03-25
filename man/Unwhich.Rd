\name{Unwhich}
\alias{Unwhich}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse Which
%%  ~~function to do ... ~~
}
\description{The inverse function to \code{\link{which}} creates a logical
vector/matrix from indices.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Unwhich(idx, n, useNames = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idx}{the indices as returned by \code{\link{which}}.
%%     ~~Describe \code{idx} here~~
}
  \item{n}{integer, the length of the original vector.
%%     ~~Describe \code{n} here~~
}
  \item{useNames}{logical, determining if the names of the indices should be preserved.
%%     ~~Describe \code{useNames} here~~
}
}
\value{a logical vector of the length n, with \code{TRUE} on the positions \code{i}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\url{https://stackoverflow.com/questions/7659833/inverse-of-which}
%% ~put references to the literature/web site here ~
}
\author{Nick Sabbe
%%  ~~who you are~~
}
\seealso{\code{\link{which}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)
names(ll) <- letters[seq(ll)]
i <- which(ll)
# back again (loosing the names of the FALSEs)
Unwhich(i, length(ll))
}
\keyword{ attribute}% __ONLY ONE__ keyword per line
\keyword{ logic}% __ONLY ONE__ keyword per line
