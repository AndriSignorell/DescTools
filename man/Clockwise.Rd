\name{Clockwise}
\alias{Clockwise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates Begin and End Angle From a List of Given Angles in Clockwise Mode
%%  ~~function to do ... ~~
}
\description{Transforms given angles in counter clock mode into clockwise angles.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Clockwise(x, start = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of angles
%%     ~~Describe \code{x} here~~
}
  \item{start}{the starting angle for the transformation. Defaults to 0.
%%     ~~Describe \code{start} here~~
}
}
\details{Sometimes there's need for angles being defined the other way round. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{a data.frame with two columns, containing the start and end angles.
}
\author{Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{PlotPolar}}
}
\examples{
Clockwise( c(0, pi/4, pi/2, pi))
}
\keyword{ dplot }
