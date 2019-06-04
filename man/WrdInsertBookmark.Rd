\name{WrdInsertBookmark}
\alias{WrdInsertBookmark}
\alias{WrdGoto}
\alias{WrdUpdateBookmark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a Bookmark, Goto Bookmark and Update the Text of a Bookmark
%%  ~~function to do ... ~~
}
\description{WrdInsertBookmark inserts a new bookmark in a Word document. WrdGotoBookmark allows to set the cursor on the bookmark and WrdUpdateBookmark sets the text within the range of the bookmark.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdInsertBookmark(name, wrd = DescToolsOptions("lastWord"))
WrdGoto(name, what = wdConst$wdGoToBookmark, wrd = DescToolsOptions("lastWord"))

WrdUpdateBookmark(name, text, what = wdConst$wdGoToBookmark,
                  wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{the name of the bookmark.
%%     ~~Describe \code{name} here~~
}
  \item{text}{the text of the bookmark.
%%     ~~Describe \code{what} here~~
}
  \item{what}{a word constant, defining the type of object to be used to place the cursor.
%%     ~~Describe \code{what} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}

\details{Bookmarks are useful to build structured documents, which can be updated later.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{WrdFont}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
wrd <- GetNewWrd()
WrdText("a)\n\n\nb)", fontname=WrdGetFont()$name, fontsize=WrdGetFont()$size)
WrdInsertBookmark("chap_b")
WrdText("\n\n\nc)\n\n\n", fontname=WrdGetFont()$name, fontsize=WrdGetFont()$size)

WrdGoto("chap_b")
WrdUpdateBookmark("chap_b", "Goto chapter B and set text")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
