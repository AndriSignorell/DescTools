\name{Midx}
\alias{Midx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the Midpoints of a Numeric Vector
%%  ~~function to do ... ~~
}
\description{Calculate the midpoints of a sequence of numbers. This is e.g. useful for labelling stacked barplots.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Midx(x, incl.zero = FALSE, cumulate = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric vector
%%     ~~Describe \code{x} here~~
}
  \item{incl.zero}{should zero be appended to x before proceeding? If \code{TRUE} the first value will be one half of the first value of x. Default is FALSE.}
  \item{cumulate}{should the result be calculated as cumulative sum? Default is FALSE.}
}
\value{numeric vector with the calculated midpoins
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{MoveAvg}}}

\examples{
x <- c(1, 3, 6, 7)

Midx(x)
Midx(x, incl.zero = TRUE)
Midx(x, incl.zero = TRUE, cumulate = TRUE)

# an alternative to
head(MoveAvg(c(0, x), order = 2, align = "l"), n = -1)

tab <- matrix(c(401,216,221,254,259,169), nrow=2, byrow=TRUE)
b <- barplot(tab, beside = FALSE, horiz=TRUE)

x <- t(apply(tab, 2, Midx, incl.zero=TRUE, cumulate=TRUE))
text(tab, x=x, y=b, col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
