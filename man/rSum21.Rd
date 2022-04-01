\name{rSum21}
\alias{rSum21}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Numbers Adding Up to 1
%%  ~~function to do ... ~~
}
\description{Generates a vector of uniformly distributed random numbers which sum to 1.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rSum21(size, digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{size}{a non-negative integer giving the number of numbers to generate.
%%     ~~Describe \code{size} here~~
}
  \item{digits}{integer indicating the number of decimal places to be used.
%%     ~~Describe \code{digits} here~~
}
}
\value{a vector of length size with elements drawn 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{runif}}, (Dirichlet distribution)
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# generate 5 numbers
x <- rSum21(5)
sum(x)
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
