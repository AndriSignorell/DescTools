\name{NALevel}
\alias{NALevel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Replace NAs in a Factor by a Given Level
%%  ~~function to do ... ~~
}
\description{In order to replace the NAs in a factor an additional level has to be defined first. 
This function does this and replaces the NAs by the given level.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NALevel(x, level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector which will be turned into a factor.
%%     ~~Describe \code{x} here~~
}
  \item{level}{the name for the new level
%%     ~~Describe \code{level} here~~
}
}
\value{the vector x with the NAs replaced by level
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{factor}}, \code{\link{levels}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(LETTERS[1:5], NA)
table(NALevel(x, "something else"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
