\name{WrdMergeCells}
\alias{WrdMergeCells}
\title{Merges Cells Of a Defined Word Table Range
%%  ~~function to do ... ~~
}
\description{Merges a cell range of a word table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdMergeCells(wtab, rstart, rend)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{a handle to the word table as returned i.g. by  \code{\link{WrdTable}}
%%     ~~Describe \code{wtab} here~~
}
  \item{rstart}{the left/upper cell of the cell range.
%%     ~~Describe \code{rstart} here~~
}
  \item{rend}{the right/lower cell of the cell range.
%%     ~~Describe \code{rend} here~~
}
}
\value{nothing
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{WrdTable}}, \code{\link{WrdCellRange}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

# Windows-specific example
wrd <- GetNewWrd()
wtab <- WrdTable(nrow=3, ncol=3, wrd=wrd)
WrdMergeCells(wtab, rstart=c(1,2), rend=c(2,3))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
\keyword{MS-Office}
