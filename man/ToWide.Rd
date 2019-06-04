\name{ToLong, ToWide}
\alias{ToWide}
\alias{ToLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reshape a Vector From Long to Wide Shape Or Vice Versa
%%  ~~function to do ... ~~
}
\description{Simple reshaping a vector from long to wide or from wide to long shape by means of a single factor.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToLong(x, varnames = NULL)
ToWide(x, g, by = NULL, varnames = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be reshaped
%%     ~~Describe \code{x} here~~
}
  \item{g}{the grouping vector to be used for the new columns. The resulting \code{data.frame} will return one column per grouplevel.
%%     ~~Describe \code{g} here~~
}
  \item{by}{a vector to be used to merge the pieces of \code{x}. If this is left to \code{NULL} the pieces will be merged by rownames in the order they are supplied.
%%     ~~Describe \code{g} here~~
}
  \item{varnames}{the variable names if not the grouping levels should be used.
%%     ~~Describe \code{varnames} here~~
}
}
\details{\code{ToLong} expects x as a matrix or a data.frame and reshapes it to a (long) factor representation.
\code{ToWide} expects the vectors x, g, by, wheras x being the variable, g the splitting factor and by a vector for rowwise merging.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the reshaped object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{reshape}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d.x <- read.table(header=TRUE, text="
AA BB CC DD EE FF GG
7.9 18.1 13.3 6.2 9.3 8.3 10.6
9.8 14.0 13.6 7.9 2.9 9.1 13.0
6.4 17.4 16.0 10.9 8.6 11.7 17.5
")

ToLong(d.x)

# ToWide by row numbers (by = NULL)
ToWide(PlantGrowth$weight, PlantGrowth$group)

# To wide aligned by key
set.seed(41)
PlantGrowth$nr <- c(sample(12, 10), sample(12, 10), sample(12, 10))
head(PlantGrowth)

ToWide(PlantGrowth$weight, PlantGrowth$group, by=PlantGrowth$nr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }
