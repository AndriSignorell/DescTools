\name{WrdTableBorders}
\alias{WrdTableBorders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Borders to a Word Table
%%  ~~function to do ... ~~
}
\description{Drawing borders in a Word table is quite tedious. This function allows to select any range and draw border lines around it.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdTableBorders(wtab, from = NULL, to = NULL, border = NULL,
                lty = wdConst$wdLineStyleSingle, col = wdConst$wdColorBlack,
                lwd = wdConst$wdLineWidth050pt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{a pointer to a Word table as returned by \code{\link{WrdTable}} or \code{\link{TOne}}.
%%     ~~Describe \code{wtab} here~~
}
  \item{from}{integer, a vector with two elements specifying the left upper bound of the cellrange.
%%     ~~Describe \code{from} here~~
}
  \item{to}{integer, a vector with two elements specifying the right bottom of the cellrange.
%%     ~~Describe \code{to} here~~
}
  \item{border}{a Word constant (\code{wdConst$wdBorder...}) defining the side of the border.
%%     ~~Describe \code{border} here~~
}
  \item{lty}{a Word constant (\code{wdConst$wdLineStyle...}) defining the line type.
%%     ~~Describe \code{lty} here~~
}
  \item{col}{a Word constant (\code{wdConst$wdColor...}) defining the color of the border. See examples for converting R colors to Word colors.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{a Word constant (\code{wdConst$wdLineWidth...pt}) defining the line width.
%%     ~~Describe \code{lty} here~~
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

# create table
tab <- table(op=d.pizza$operator, area=d.pizza$area)

# send it to Word
wrd <- GetNewWrd()
wtab <- ToWrd(tab, wrd=wrd, tablestyle = NA)

# draw borders
WrdTableBorders(wtab, from=c(2,2), to=c(3,3), border=wdConst$wdBorderBottom, wrd=wrd)
WrdTableBorders(wtab, from=c(2,2), to=c(3,3), border=wdConst$wdBorderDiagonalUp, wrd=wrd)

# demonstrate linewidth and color
wtab <- ToWrd(tab, wrd=wrd, tablestyle = NA)
WrdTableBorders(wtab, col=RgbToLong(ColToRgb("olivedrab")),
                lwd=wdConst$wdLineWidth150pt, wrd=wrd)

WrdTableBorders(wtab, border=wdConst$wdBorderBottom,
                col=RgbToLong(ColToRgb("dodgerblue")),
                lwd=wdConst$wdLineWidth300pt, wrd=wrd)

# use an R color in Word
RgbToLong(ColToRgb("olivedrab"))

# find a similar R-color for a Word color
ColToRgb(RgbToCol(LongToRgb(wdConst$wdColorAqua)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print}
\concept{MS-Office}
