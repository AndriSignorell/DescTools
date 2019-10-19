\name{XLSaveAs}
\alias{XLSaveAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Save Excel File
%%  ~~function to do ... ~~
}
\description{Save the current workbook under the given name and format.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
XLSaveAs(fn, file_format = xlConst$XlFileFormat$xlWorkbookNormal, 
         xl = DescToolsOptions("lastXL"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{the filename
%%     ~~Describe \code{fn} here~~
}
  \item{file_format}{the file format using the xl constant.
%%     ~~Describe \code{file_format} here~~
}
\item{xl}{the pointer to a MS-Excel instance. An new instance can be created with \code{GetNewXL()}, returning the appropriate handle. A handle to an already running instance is returned by \code{GetCurrXL()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastXL")}.}
}
\value{returns \code{TRUE} if the save operation has been successful
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{XLView}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{# Windows-specific example
XLView(d.diamonds)
XLSaveAs("Diamonds")
xl$quit()
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{MS-Office}
