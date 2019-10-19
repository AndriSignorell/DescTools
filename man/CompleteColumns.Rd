\name{CompleteColumns}
\alias{CompleteColumns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find Complete Columns
%%  ~~function to do ... ~~
}
\description{Return either the columnnames or a logical vector indicating which columns are complete, i.e., have no missing values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CompleteColumns(x, which = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame containing the data
%%     ~~Describe \code{x} here~~
}
  \item{which}{logical, determining if the names of the variables should be returned or a if a logical vector indicating which columns are complete should be returned.
%%     ~~Describe \code{which} here~~
}
}
\value{
A logical vector specifying which columns have no missing values across the entire sequence.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{is.na}},  \code{\link{na.omit}},  \code{\link{complete.cases}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
CompleteColumns(d.pizza)
CompleteColumns(d.pizza, which=FALSE)
}
\keyword{ NA }% __ONLY ONE__ keyword per line
\keyword{ logic }% __ONLY ONE__ keyword per line
