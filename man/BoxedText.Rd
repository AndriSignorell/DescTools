\name{BoxedText}
\alias{BoxedText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Text in a Box to a Plot
%%  ~~function to do ... ~~
}
\description{BoxedText draws the strings given in the vector labels at the coordinates given by x and y, surrounded by a rectangle.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BoxedText(x, y = NULL, labels = seq_along(x), adj = NULL, pos = NULL, offset = 0.5, 
          vfont = NULL, cex = 1, txt.col = NULL, font = NULL, srt = 0, 
          xpad = 0.2, ypad = 0.2, density = NULL, angle = 45, col = "white", 
          border = par("fg"), lty = par("lty"), lwd = par("lwd"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{numeric vectors of coordinates where the text labels should be written. If the length of x and y differs, the shorter one is recycled.
%%     ~~Describe \code{x} here~~
}
  \item{labels}{a character vector or expression specifying the text to be written. 
  An attempt is made to coerce other language objects (names and calls) to expressions, and vectors and other 
  classed objects to character vectors by as.character. If labels is longer than x and y, the coordinates are recycled to the length of labels.
%%     ~~Describe \code{labels} here~~
}
  \item{adj}{The value of adj determines the way in which text strings are justified. 
  A value of 0 produces left-justified text, 0.5 (the default) centered text and 1 right-justified text. 
  (Any value in [0, 1] is allowed, and on most devices values outside that interval will also work.) 
  Note that the adj argument of text also allows adj = c(x, y) for different adjustment in x- and y- directions. 

%%     ~~Describe \code{adj} here~~
}
  \item{pos}{a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
%%     ~~Describe \code{pos} here~~
}
  \item{offset}{when pos is specified, this value gives the offset of the label from the specified coordinate in fractions of a character width.
%%     ~~Describe \code{offset} here~~
}
  \item{vfont}{\code{NULL} for the current font family, or a character vector of length 2 for Hershey vector fonts. The first element of the vector 
   selects a typeface and the second element selects a style. Ignored if labels is an expression.
%%     ~~Describe \code{vfont} here~~
}
  \item{cex}{numeric character expansion factor; multiplied by \code{par("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to 1.0.
%%     ~~Describe \code{cex} here~~
}
  \item{txt.col, font}{the color and (if vfont = NULL) font to be used, possibly vectors. These default to the values of the global graphical parameters in \code{par()}.
%%     ~~Describe \code{txt.col} here~~
}
  \item{srt}{The string rotation in degrees. 
%%     ~~Describe \code{srt} here~~
}
  \item{xpad, ypad}{The proportion of the rectangles to the extent of the text within.
%%     ~~Describe \code{xpad} here~~
}
  \item{density}{the density of shading lines, in lines per inch. The default value of \code{NULL} means that no shading lines are drawn. 
  A zero value of density means no shading lines whereas negative values (and NA) suppress shading (and so allow color filling).
%%     ~~Describe \code{density} here~~
}
  \item{angle}{angle (in degrees) of the shading lines.
%%     ~~Describe \code{angle} here~~
}
  \item{col}{color(s) to fill or shade the rectangle(s) with. The default \code{NA} (or also NULL) means do not fill, 
  i.e., draw transparent rectangles, unless density is specified.
%%     ~~Describe \code{col} here~~
}
  \item{border}{color for rectangle border(s). The default is \code{par("fg")}. Use \code{border = NA} to omit borders 
  (this is the default). 
  If there are shading lines, \code{border = TRUE} means use the same colour for the border as for the shading lines.
%%     ~~Describe \code{border} here~~
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width for borders and shading. Note that the use of \code{lwd = 0} (as in the examples) is device-dependent.
%%     ~~Describe \code{lwd} here~~
}
  \item{\dots}{additional arguments are passed to the text function.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{SpreadOut}}, similar function in package \pkg{plotrix} \code{\link[plotrix]{boxed.labels}} (lacking rotation option)
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Canvas(xpd=TRUE)

BoxedText(0, 0, adj=0, label="This is boxed text", srt=seq(0,360,20), xpad=.3, ypad=.3)
points(0,0, pch=15)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ misc }