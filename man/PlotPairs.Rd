\name{PlotPairs}
\alias{PlotPairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extended Scatterplot Matrices
%%  ~~function to do ... ~~
}
\description{A matrix of scatterplots is produced.The upper triangular matrices contain nothing else than the correlation coefficient. The diagonal displays a histogram of the variable. The lower triangular matrix displays a scatterplot superposed by a smoother. It's possible to define groups to be differntiated by color and also by individual smoothers.
The used code is not much more than the \code{\link{pairs}()} code and some examples, but condenses it to a practical amount.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPairs(x, g = NULL, col = 1, pch = 19, col.smooth = 1, main = "", 
          upper = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the coordinates of points given as numeric columns of a matrix or data frame. Logical and factor columns are converted to numeric in the same way that data.matrix does. Will directly be passed on to \code{pairs}.
%%     ~~Describe \code{x} here~~
}
  \item{g}{a group variable
%%     ~~Describe \code{g} here~~
}
  \item{col}{color for pointcharacter
%%     ~~Describe \code{col} here~~
}
  \item{pch}{point character
%%     ~~Describe \code{pch} here~~
}
  \item{col.smooth}{color for the smoother(s)
%%     ~~Describe \code{col.smooth} here~~
}
  \item{main}{the main title
%%     ~~Describe \code{main} here~~
}
\item{upper}{logical, determines if the correlation coefficients should be displayed in the upper triangular matrix (default) or in the lower one.}
  \item{\dots}{additional arguments passed to \code{pairs} function.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{pairs}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotPairs(iris[1:4], g=iris$Species, main = "Anderson's Iris Data -- 3 species",
          col=c(hred, hblue, hgreen), col.smooth=c("black", hred, hblue, hgreen))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% __ONLY ONE__ keyword per line
