\name{Str}
\alias{Str}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compactly Display the Structure of any R Object
%%  ~~function to do ... ~~
}
\description{Basically a wrapper for \code{\link{str}()}, extended with an enumeration for the variables of a data.frame.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Str(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{any \code{R} object about which you want to have some information.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{dots are passed to \code{\link{str}}.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{str}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Str(d.pizza)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
