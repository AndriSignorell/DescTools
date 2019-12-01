\name{ToWrdPlot}
\alias{ToWrdPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send a Plot to Word and Bookmark it
%%  ~~function to do ... ~~
}
\description{Evaluate given plot code to a \code{\link{tiff}()} device and imports the created plot in the currently open MS-Word document. The imported plot is marked with a bookmark that can later be used for a potential update (provided the bookmark name has been stored).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToWrdPlot(plotcode, width = NULL, height = NULL, scale = 100,
          res = 300, crop = 0, title = NULL,  
          wrd = DescToolsOptions("lastWord"), 
          bookmark = gettextf("bmp\%s", round(runif(1, min = 0.1) * 1e+09)))
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
  \item{crop}{a vector of 4 elements, the crop factor for all 4 sides of a picture in cm (default all 0)
%%     ~~Describe \code{res} here~~
}
  \item{title}{character, the title of the plot to be inserted in the word document
}
 \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}

}
\details{An old and persistent problem that has existed for a long time is that as results  once were loaded into a Word document the connection broke so that no update was possible. It was only recently that I realized that bookmarks in Word could be a solution for this.
The present function evaluates some given plot code chunk using a tiff device and imports the created plot in a word document. The imported plot is given a bookmark, that can be used afterwards for changing or updating the plot. 

This function is designed for use with the \bold{DescToolsAddIns} functions \code{ToWrdPlotWithBookmark()} and \code{ToWrdWithBookmark()} allowing to assign keyboard shortcuts. The two functions will also insert the newly defined bookmark in the source file in a format, which can be interpreted by the function \code{UpdateBookmark()}.
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
