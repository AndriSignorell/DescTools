\name{ToWrdB}
\alias{ToWrdB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send Objects to Word and Bookmark Them
%%  ~~function to do ... ~~
}
\description{Send objects like tables, ftables, lm tables, TOnes or just simple texts to a MS-Word document and place a bookmark on them. This has the advantage, that objects in a Word document can be updated later, provided the bookmark name has been stored.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToWrdB(x, font = NULL, ..., wrd = DescToolsOptions("lastWord"), 
        bookmark = gettextf("b\%s", sample(1e9, 1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be transferred to Word.
%%     ~~Describe \code{x} here~~
}
  \item{font}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE)}.
%%     ~~Describe \code{font} here~~
}
   \item{\dots}{further arguments to be passed to or from methods.
%%     ~~Describe \code{\dots} here~~
}
 \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}
  \item{bookmark}{the name of the bookmark.
%%     ~~Describe \code{bookmark} here~~
}
}
\details{This function encapsulates \code{\link{ToWrd}}, by placing a bookmark over the complete inserted results.
The given name can be questioned with \code{bm$name()}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a handle to the set bookmark
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

\seealso{\code{\link{ToWrd}}, \code{\link{WrdInsertBookmark}}
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
% use one of  RShowDoc("KEYWORDS")
