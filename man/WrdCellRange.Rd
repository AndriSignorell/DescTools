\name{WrdCellRange}
\alias{WrdCellRange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return the Cell Range Of a Word Table
%%  ~~function to do ... ~~
}
\description{Return a handle of a cell range of a word table. This is useful for
formating the cell range.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdCellRange(wtab, from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{a handle to the word table as returned i.g. by  \code{\link{WrdTable}}
%%     ~~Describe \code{wtab} here~~
}
  \item{from}{a vector containing row- and column number of the left/upper cell of the cell range.
%%     ~~Describe \code{rstart} here~~
}
  \item{to}{a vector containing row- and column number of the right/lower cell of the cell range.
%%     ~~Describe \code{rend} here~~
}
}
\details{Cell range selecting might be complicated. This function makes it easy.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a handle to the range.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

# Windows-specific example
wrd <- GetNewWrd()
WrdTable(nrow=3, ncol=3, wrd=wrd)
crng <- WrdCellRange(from=c(1,2), to=c(2,3))
crng$Select()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }
\keyword{MS-Office}
