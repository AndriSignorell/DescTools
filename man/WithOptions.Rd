\name{WithOptions}
\alias{WithOptions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Execute Function with Temporary Options
%%  ~~function to do ... ~~
}
\description{Setting and resetting options is lengthy in command mode. \code{WithOptions()} allows to evaluate a function with temporary set options. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WithOptions(optlist, expr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{optlist}{a list with new option settings.
%%     ~~Describe \code{optlist} here~~
}
  \item{expr}{the expression to be evaluated
%%     ~~Describe \code{expr} here~~
}
}
\value{the function result 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Thomas Lumley <t.lumley@auckland.ac.nz>
%%  ~~who you are~~
}

\seealso{\code{\link{options}}, \code{\link{getOption}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# original:
print((1:10)^-1)

# with new options
WithOptions(list(digits=3), print((1:10)^-1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ "environment" }  % use one of  RShowDoc("KEYWORDS")
