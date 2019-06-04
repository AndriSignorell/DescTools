\name{CatTable}
\alias{CatTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to write a table
%%  ~~function to do ... ~~
}
\description{CatTable helps printing a table, if is has to be broken into multiple rows. Rowlabels will be
repeated after every new break.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CatTable(tab, wcol, nrepchars, width = getOption("width"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{the rows of a table to be printed, pasted together in one string with constant columnwidth.
%%     ~~Describe \code{tab} here~~
}
  \item{wcol}{integer, the width of the columns. All columns must have the same width.
%%     ~~Describe \code{wcol} here~~
}
  \item{nrepchars}{integer, the number of characters to be repeated with every break. This 
  is typically the maximum width of the rowlabels.
%%     ~~Describe \code{nrepchars} here~~
}
  \item{width}{integer, the width of the whole table. Default is the width of the current command window 
  (\code{getOption("width")}).
%%     ~~Describe \code{width} here~~
}
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{table}}, \code{\link{paste}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# used in bivariate description functions
Desc(temperature ~ cut(delivery_min, breaks=40), data=d.pizza)


txt <- c( 
   paste(sample(letters, 500, replace=TRUE), collapse="")
 , paste(sample(letters, 500, replace=TRUE), collapse="")
 , paste(sample(letters, 500, replace=TRUE), collapse="")
)
txt <- paste(c("aaa","bbb","ccc"), txt, sep="")

CatTable(txt, nrepchars=3, wcol=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }


