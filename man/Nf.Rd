\name{Nf}
\alias{Nf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{As Numeric Factor
%%  ~~function to do ... ~~
}
\description{Encode a vector x to a factor and then to a numeric value. It's a simple shortcut for \code{as.numeric(factor(x, ...))}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Nf(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{	
a vector of data, usually taking a small number of distinct values.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{the dots are passed on to \code{\link{factor}}
%%     ~~Describe \code{\dots} here~~
}
}
\value{numeric vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{N}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- LETTERS[10:15]
Nf(x)

# same as ..
as.numeric(factor(x))
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
