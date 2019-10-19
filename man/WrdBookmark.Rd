\name{WrdBookmark}
\alias{WrdBookmark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return a Handle to a Word Bookmark Given as Name
%%  ~~function to do ... ~~
}
\description{Accessing bookmarks by name is only possible by browsing the bookmark names. This function returns a handle to a bookmark by name.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdBookmark(bookmark, wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bookmark}{character, the bookmark name 
%%     ~~Describe \code{bookmark} here~~
}
 \item{wrd}{the pointer to a word instance, typically an existing one, created by \code{GetCurrWrd()}. Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}
}
\value{a handle to the bookmark
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{WrdInsertBookmark}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# we can't get this through the CRAN test - run it with copy/paste to console

wrd <- GetNewWrd()
WrdInsertBookmark("mybookmark")
ToWrd("A longer text\n\n\n")

# Now returning the bookmark
bm <- WrdBookmark("mybookmark")

# get the automatically created name of the bookmark
bm$name()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }% __ONLY ONE__ keyword per line
\keyword{MS-Office}

