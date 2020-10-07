\name{TextToTable}
\alias{TextToTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts String To a Table
%%  ~~function to do ... ~~
}
\description{Try to convert a string to a table, by first creating a data frame using \code{\link{read.table}}. This can then be coerced to a matrix first, and subsequently to a table. The names of the dimensions can be specified.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TextToTable(x, dimnames = NULL, check.names = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the string to be interpreted as table.
%%     ~~Describe \code{x} here~~
}
  \item{dimnames}{the names of the dimensions.
%%     ~~Describe \code{dimnames} here~~
}
\item{check.names}{passed on to \code{\link{read.table}} and determines, if invalid column names should be adapted to valid ones. The default here is changed to \code{FALSE}.}
  \item{\dots}{the dots will be passed to the function \code{\link{read.table}} and can be used for example to specify \code{header}, \code{sep} and \code{row.names} arguments.
%%     ~~Describe \code{\dots} here~~
}
}
\value{a table
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{read.table}}, \code{\link{as.table}}, \code{\link{as.matrix}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
txt <- "
    Democrat, Independent, Republican
  M, 762, 327, 468
  F, 484, 239, 477"

(tab <- TextToTable(txt, header=TRUE, sep=",", dimnames=c("gender", "party")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }% __ONLY ONE__ keyword per line
