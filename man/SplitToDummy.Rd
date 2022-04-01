\name{SplitToDummy}
\alias{SplitToDummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split Strings of a Vector and Provide Dummy Codes for Found Pieces
%%  ~~function to do ... ~~
}
\description{Split the strings of a character vector, put together all the unique pieces and return a matrix of dummy vectors for each single value.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SplitToDummy(x, split = ",", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character vector, each element of which is to be split. Other inputs, including a factor, will give an error.
%%     ~~Describe \code{x} here~~
}
  \item{split}{character vector (or object which can be coerced to such) containing regular expression(s) (unless \code{fixed = TRUE}) to use for splitting. If empty matches occur, in particular if \code{split} has length 0, \code{x} is split into single characters. If \code{split} has length greater than 1, it is re-cycled along x.
%%     ~~Describe \code{split} here~~
}
\item{dots}{the dots are passed on to \code{\link{strsplit}}}
}
\value{a data.frame containing \code{x} and all the found dummy vectors
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{strsplit}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d.frm <- data.frame(id=1:5, txt=c("A,C,D", "A","B,C","D","D,E"))
SplitToDummy(d.frm$txt)
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
