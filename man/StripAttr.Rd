\name{StripAttr}
\alias{StripAttr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove Attributes from an Object
%%  ~~function to do ... ~~
}
\description{For convenience we sometimes want to strip some or all attributes in a oneliner. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StripAttr(x, attr_names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object whose attributes should be removed.
%%     ~~Describe \code{x} here~~
}
  \item{attr_names}{a vector with attribute names, which will be removed. Leaving the default to \code{NULL} will cause all the attributes to be deleted.
%%     ~~Describe \code{attr_names} here~~
}
}
\value{the object \code{x} without the attributes contained in attr_names
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{SetNames}}, \code{\link{unname}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- runif(10)
attr(x, "some_attr") <- "First attribute"
attr(x, "other_attr") <- "Second attribute"

# strip all attributes
StripAttr(x)

# only some
StripAttr(x, "other_attr")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ manipulation }
% \concept{ ~cpt2 }
% Use only one concept per line.


