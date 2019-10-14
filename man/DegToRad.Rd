\name{DegToRad}
\alias{DegToRad}
\alias{RadToDeg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Degrees to Radians and Vice Versa
%%  ~~function to do ... ~~
}
\description{Convert degrees to radians (and back again).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DegToRad(deg)
RadToDeg(rad)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deg}{a vector of angles in degrees.
%%     ~~Describe \code{deg} here~~
  }
  \item{rad}{a vector of angles in radians.
%%     ~~Describe \code{deg} here~~
  }
}
\value{DegToRad returns a vector of the same length as \code{deg} with the angles in radians.\cr
RadToDeg returns a vector of the same length as \code{rad} with the angles in degrees.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
DegToRad(c(90,180,270))
RadToDeg( c(0.5,1,2) * pi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
