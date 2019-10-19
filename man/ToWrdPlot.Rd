\name{ToWrdPlot}
\alias{ToWrdPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send a Plot to Word and Bookmark it
%%  ~~function to do ... ~~
}
\description{Evaluate given plot code and sends the resulting plot to a MS-Word document and adds a bookmark to it. This has the advantage, that the plot in the Word document can be updated later, provided the bookmark name has been stored.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToWrdPlot(plotcode, bookmark = NULL, width = NULL, height = NULL, 
          scale = 100, res = 300, wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plotcode}{code chunk needed for producing the plot
%%     ~~Describe \code{plotcode} here~~
}
  \item{bookmark}{character, the name of the bookmark
%%     ~~Describe \code{bm} here~~
}
  \item{width}{the width in cm of the plot in the Word document (default 15)
%%     ~~Describe \code{width} here~~
}
  \item{height}{the height in cm of the plot in the Word document (default 9.3)
%%     ~~Describe \code{height} here~~
}
  \item{scale}{the scale of the plot (default 100) 
%%     ~~Describe \code{scale} here~~
}
  \item{res}{the resolution for the graphic (default 300)
%%     ~~Describe \code{res} here~~
}
 \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}

}
\details{An old and persistent problem that has existed for a long time is that as results  once were loaded into a Word document the connection broke so that no update was possible. It was only recently that I realized that bookmarks in Word could be a solution for this.
The present function evaluates some given plot code chunk using a tiff device and imports the created plot in a word document. The imported plot is give a bookmark, that can be used afterwards for changing or updating the plot. 

This function is designed for use with the \bold{DescToolsAddIns} functions \code{ToWrdPlotWithBookmark()} and \code{ToWrdWithBookmark()} allowing to assign keyboard shortcuts.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a list
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{plot_hwnd }{a windows handle to the inserted plot}
\item{bookmark}{a windows handle to the bookmark}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ToWrdB}}, \code{\link{WrdInsertBookmark}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# we can't get this through the CRAN test - run it with copy/paste to console

wrd <- GetNewWrd()
bm <- ToWrdB("This is text to be possibly replaced later.")

# get the automatically created name of the bookmark
bm$name()

WrdGoto(bm$name())
UpdateBookmark(...)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
