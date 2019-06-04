\name{ColumnWrap}
\alias{ColumnWrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ColumnWrap
%%  ~~function to do ... ~~
}
\description{Wraps text in a character matrix so, that it's displayed over more than one line.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ColumnWrap(x, width = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the matrix with one row
%%     ~~Describe \code{x} here~~
}
  \item{width}{integer, the width of the columns in characters
%%     ~~Describe \code{width} here~~
}
}
\details{A data.frame containing character columns with long texts is often wrapped by columns. This can lead to a loss of overview. ColumnWrap wraps the lines within the columns.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a character matrix
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{strwrap}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Abstract(d.pizza)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }% use one of  RShowDoc("KEYWORDS")
