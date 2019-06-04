\name{\%like\%}
\alias{\%like\%}
\alias{\%like any\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Like Operator
%%  ~~function to do ... ~~
}
\description{The like operator is a simple wrapper for \code{\link{grep}(..., value=TRUE)}, whose complexity is hard to crack for R-newbies.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
x \%like\% pattern

x \%like any\% pattern
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, typically of character or factor type
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{simple character string to be matched in the given character vector.
%%     ~~Describe \code{pattern} here~~
}
}
\details{Follows the logic of simple SQL or basic commands.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ a vector (numeric, character, factor), matching the mode of \code{x}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{match}}, \code{\link{pmatch}}, \code{\link{grep}}, \code{\link{\%[]\%}}, \code{\link{\%overlaps\%}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# find names ending on "or"
names(d.pizza) \%like\% "\%or"

# find names starting with "d"
names(d.pizza) \%like\% "d\%"

# ... containing er?
names(d.pizza) \%like\% "\%er\%"

# and combined, search for a name containing "un", ending on "or"
# or beginning with "F"
levels(d.pizza$driver) \%like any\% c("\%un\%", "\%or", "F\%")

# the positions on the vector
match(names(d.pizza) \%like\% "\%er\%", names(d.pizza))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
