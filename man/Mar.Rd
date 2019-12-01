\name{Mar and Mgp}
\alias{Mar}
\alias{Mgp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set Plot Margins and Distances
%%  ~~function to do ... ~~
}
\description{Plot margins are normally set by \code{par("mar")}. However one is forced to define all margins, even if just one should be altered. The convenience function \code{Mar()} allows to set one single margin (or several) while leaving the others unchanged.

\code{Mgp()} does the same for the distances of axis title, labels and line.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mar(bottom = NULL, left = NULL, top = NULL, right = NULL, outer = FALSE)
Mgp(title = NULL, labels = NULL, line = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bottom}{the bottom margin, if set to \code{NULL} the current value will be maintained.
%%     ~~Describe \code{bottom} here~~
}
  \item{left}{the left margin, if set to \code{NULL} the current value will be maintained.
%%     ~~Describe \code{left} here~~
}
  \item{top}{the top margin, if set to \code{NULL} the current value will be maintained.
%%     ~~Describe \code{top} here~~
}
  \item{right}{the right margin, if set to \code{NULL} the current value will be maintained.
%%     ~~Describe \code{right} here~~
}
  \item{outer}{logical, defining if inner margins (\code{par("mar")}) or the outer margins (\code{par("oma")}) should be set. Default is \code{FALSE}, meaning that the inner margins will be concerned.
%%     ~~Describe \code{outer} here~~
}
\item{title}{margin line for the axis title (default 3)}
\item{labels}{margin line for the axis labels (default 1)}
\item{line}{margin line for the axis line (default 0)}
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{par}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# largen the left margin only
Mar(left=10.1)   # or as alternative: Mar(, 10.1)
Mgp(title=6)     # ylab must be placed a little further to the left
barplot(1:7, names=levels(d.pizza$driver), horiz=TRUE, las=1, 
        ylab="driver", col=Pal("Helsana"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities }
