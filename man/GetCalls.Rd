\name{GetCalls}
\alias{GetCalls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return All Used Functions Within a Function
%%  ~~function to do ... ~~
}
\description{For screening purposes it can be useful to get a list of all function calls our function may depend on. \code{GetCall()} parses the function source and return all found function calls grouped by their package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetCalls(fun, alphabetic = TRUE, package = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{the name of the function to be parsed
%%     ~~Describe \code{fun} here~~
}
  \item{alphabetic}{logic, determining the order of the result
%%     ~~Describe \code{alphabetic} here~~
}
  \item{package}{name of the package, if only functions of this specific package should be returned.
%%     ~~Describe \code{package} here~~
}
}
\value{a list of vectors structered by package 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Nicholas Cooper <njcooper at gmx.co.uk> (in package NCmisc)
with some tweaking by Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{LsFct}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
GetCalls("t.test.default")

sapply(c("Closest", "Format"), 
       function(x) paste(unname(unlist(GetCalls(x))), collapse=", "))
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
