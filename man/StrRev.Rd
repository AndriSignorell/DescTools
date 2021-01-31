\name{StrRev}
\alias{StrRev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reverse a String
%%  ~~function to do ... ~~
}
\description{Returns a string in reverse order.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrRev(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a string to be processed.
%%     ~~Describe \code{x} here~~
}
}
\value{string
}
\author{
Andri Signorell <andri@signorell.net> solely copying R core code from strsplit example} 

\seealso{ String functions: 
\code{\link{nchar}}, \code{\link{match}}, \code{\link{grep}}, \code{\link{regexpr}}, 
\code{\link{substr}}, \code{\link{sub}}, \code{\link{gsub}},
\code{\link{StrTrunc}}, \code{\link{StrDist}}  

%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
StrRev("home")
StrRev("Anna")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character }
\concept{ string }
