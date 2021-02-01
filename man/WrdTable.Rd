\name{WrdTable}
\alias{WrdTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a Table in a Word Document
%%  ~~function to do ... ~~
}
\description{Create a table with a specified number of rows and columns in a Word document at the current position of the cursor.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdTable(nrow = 1, ncol = 1, heights = NULL, widths = NULL, main = NULL,
         wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrow}{number of rows.
%%     ~~Describe \code{nrow} here~~
}
  \item{ncol}{number of columns.
%%     ~~Describe \code{ncol} here~~
}
  \item{heights}{a vector of the row heights (in [cm]). If set to \code{NULL} (which is the default) the Word defaults will be used. The values will be recyled, if necessary.
%%     ~~Describe \code{ncol} here~~
}
  \item{widths}{a vector of the column widths (in [cm]). If set to \code{NULL} (which is the default) the Word defaults will be used. The values will be recyled, if necessary.
%%     ~~Describe \code{ncol} here~~
}
\item{main}{a caption for the plot. This will be inserted by InserCaption in Word. Default is NULL, which will insert nothing.}

\item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
}
}
\value{A pointer to the inserted table.
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

\seealso{\code{\link{GetNewWrd}}, \code{\link{ToWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
wrd <- GetNewWrd()
WrdTable(nrow=3, ncol=3, wrd=wrd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }
\concept{MS-Office}
