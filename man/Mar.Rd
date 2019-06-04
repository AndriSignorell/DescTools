\name{Mar}
\alias{Mar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set Plot Margins
%%  ~~function to do ... ~~
}
\description{Plot margins are normally set by \code{par("mar")}. However one is forced to define all margins, even if just one should be altered. This convenience function allows to set one single margin (or several) while leaving the others unchanged.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mar(bottom = NULL, left = NULL, top = NULL, right = NULL, outer = FALSE)
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
  \item{outer}{logical, defining if inner margins (\code{par("mar")}) or the outer margins (\code{par("oma")}) should be set. Default is FALSE, meaning that the inner margins will be concerned.
%%     ~~Describe \code{outer} here~~
}
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
barplot(1:7, names=levels(d.pizza$driver))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities }
