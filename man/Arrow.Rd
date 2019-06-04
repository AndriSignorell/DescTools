\name{Arrow}
\alias{Arrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert an Arrow Into a Plot
%%  ~~function to do ... ~~
}
\description{Places an arrow into a plot. Two additional arrow heads are implemented.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arrow(x0, y0, x1, y1, col = par("bg"), border = par("fg"), head = 1,
      cex = 1, lwd = 1, lty = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0, y0}{coordinates of points \bold{from} which to draw.
%%     ~~Describe \code{x0} here~~
}
  \item{x1, y1}{coordinates of points \bold{to} which to draw. At least one must the supplied.
%%     ~~Describe \code{y0} here~~
}
  \item{col}{the color of the line and background color of the arrow head.
%%     ~~Describe \code{col} here~~
}
  \item{border}{color of the arrow head.
%%     ~~Describe \code{col} here~~
}\item{head}{type of head, can be one out of 1:3.
%%     ~~Describe \code{head} here~~
}
  \item{cex}{extension factor for the arrow head.
%%     ~~Describe \code{cex} here~~
}
  \item{lwd}{line width
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type
%%     ~~Describe \code{lty} here~~
}
}

\value{Nothing returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Andri Signorell <andri@signorell.net>}


\seealso{\code{\link{arrows}}
}

\examples{
Canvas()
Arrow(1, 1, 0, 0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot}
