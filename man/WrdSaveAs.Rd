\name{WrdSaveAs}
\alias{WrdSaveAs}
\alias{WrdOpenFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Open and Save Word Documents
%%  ~~function to do ... ~~
}
\description{Open and save MS-Word documents.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdOpenFile(fn, wrd = DescToolsOptions("lastWord"))
WrdSaveAs(fn, fileformat = "docx", wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{filename and -path for the document.
%%     ~~Describe \code{fn} here~~
}
  \item{fileformat}{file format, one out of \code{"doc"}, \code{"htm"}, \code{"pdf"}.
%%     ~~Describe \code{fileformat} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}

}
\value{nothing returned
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

\seealso{\code{\link{GetNewWrd}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
#   Windows-specific example
wrd <- GetNewWrd()
WrdCaption("A Report")
WrdSaveAs(fn="report", fileformat="htm")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}% __ONLY ONE__ keyword per line
\concept{MS-Office}
