\name{WrdFormatCells}
\alias{WrdFormatCells}
\title{Format Cells Of a Word Table
%%  ~~function to do ... ~~
}
\description{Format cells of a Word table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdFormatCells(wtab, rstart, rend, col = NULL, bg = NULL,
               font = NULL, border = NULL, align = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{a handle to the word table as returned i.g. by  \code{\link{WrdTable}}
%%     ~~Describe \code{wtab} here~~
}
  \item{rstart}{the left/upper cell of the cell range
%%     ~~Describe \code{rstart} here~~
}
  \item{rend}{the right/lower cell of the cell range
%%     ~~Describe \code{rend} here~~
}
  \item{col}{the foreground colour
%%     ~~Describe \code{rend} here~~
}
  \item{bg}{the background colour
%%     ~~Describe \code{rend} here~~
}
  \item{font}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE, color=wdConst$wdColorBlack)}.
%%     ~~Describe \code{rend} here~~
}
  \item{border}{the border of the cell range, defined as a list containing arguments for
  border, linestyle, linewidth and color. \code{border} is a vector containing the parts of the border defined by the Word constants \code{wdConst$wdBorder...},  being
  $wdBorderBottom, $wdBorderLeft, $wdBorderTop, $wdBorderRight, $wdBorderHorizontal,
$wdBorderVertical, $wdBorderDiagonalUp, $wdBorderDiagonalDown. linestyle, linewidth and color will be recycled to the required dimension.
%%     ~~Describe \code{rend} here~~
}
  \item{align}{a character out of \code{"l"}, \code{"c"}, \code{"r"} setting the horizontal alignment of the cell range.
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
\dontrun{   # Windows-specific example

m <- matrix(rnorm(12)*100, nrow=4,
            dimnames=list(LETTERS[1:4], c("Variable","Value","Remark")))

wrd <- GetNewWrd()
wt <- ToWrd(m)

WrdFormatCells(wt, rstart=c(3,1), rend=c(4,3),
               bg=wdConst$wdColorGold, font=list(name="Arial Narrow", bold=TRUE),
               align="c", border=list(color=wdConst$wdColorTeal,
                                      linewidth=wdConst$wdLineWidth300pt))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }
